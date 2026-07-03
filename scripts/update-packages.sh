#!/usr/bin/env bash
set -euo pipefail

SYSTEM=$(nix eval --impure --expr 'builtins.currentSystem' --raw)

# Recursively collect all packages (at any nesting depth) that declare
# passthru.updateScript, using dotted attribute paths (e.g. claude.foo).
PACKAGES=$(nix eval ".#packages.$SYSTEM" --apply '
  let
    # Recursively collect dotted attr paths of all packages with passthru.updateScript.
    collect = attrPath: pkgSet:
      builtins.concatLists (map (name:
        let
          pkg = pkgSet.${name};
          # Build the dotted path, e.g. "claude" + "foo" -> "claude.foo".
          fullPath = if attrPath == "" then name else attrPath + "." + name;
          # Derivations have outPath; plain attrsets (namespaces) do not.
          isDerivation = builtins.isAttrs pkg && pkg ? outPath;
          isManaged = (pkg.passthru or {}) ? updateScript;
        in
          if !builtins.isAttrs pkg then []                                    # skip non-attrsets
          else if isDerivation then (if isManaged then [ fullPath ] else [])
          else collect fullPath pkg                                            # recurse into namespace
      ) (builtins.attrNames pkgSet));
  in
  collect ""
' --json | jq -r '.[]')

if [ -z "$PACKAGES" ]; then
  echo "No managed packages found"
  exit 0
fi

# nix-update extracts the updateScript with `with import <nixpkgs> {}`, so
# NIX_PATH must point to the nixpkgs revision locked in the flake.
export NIX_PATH="nixpkgs=$(nix eval --impure --raw --expr '(builtins.getFlake "path:.").inputs.nixpkgs.outPath')"

for PKG in $PACKAGES; do
  echo "::group::Checking $PKG"

  CURRENT=$(nix eval ".#packages.$SYSTEM.$PKG.version" --raw)

  nix-update --flake --use-update-script "$PKG" || true

  # Derive the changed directory from git rather than computing it from the
  # attribute name, which may not match the actual file-system path.
  CHANGED_FILES=$(git diff --name-only -- pkgs/)
  if [ -z "$CHANGED_FILES" ]; then
    echo "$PKG is up to date ($CURRENT)"
    echo "::endgroup::"
    continue
  fi
  PKG_DIR=$(dirname "$(echo "$CHANGED_FILES" | head -1)")

  LATEST=$(nix eval ".#packages.$SYSTEM.$PKG.version" --raw)
  PKG_SLUG=$(echo "$PKG" | tr '.' '-')
  BRANCH="auto/$PKG_SLUG-$LATEST"

  if git ls-remote --exit-code origin "refs/heads/$BRANCH" > /dev/null 2>&1; then
    echo "PR for $PKG $LATEST already exists, skipping"
    git checkout -- "$PKG_DIR"
    echo "::endgroup::"
    continue
  fi

  # Close any stale PRs opened by this script for older versions
  STALE_BRANCHES=$(git ls-remote origin "refs/heads/auto/$PKG_SLUG-*" | awk '{print $2}' | sed 's|refs/heads/||')
  for OLD_BRANCH in $STALE_BRANCHES; do
    PR_NUMBER=$(curl -s --fail-with-body "${FORGEJO_SERVER_URL}/api/v1/repos/${FORGEJO_REPOSITORY}/pulls?state=open&limit=50" \
      -H "Authorization: token ${FORGEJO_TOKEN}" \
      | jq -r ".[] | select(.head.ref == \"$OLD_BRANCH\") | .number")
    if [ -n "$PR_NUMBER" ]; then
      echo "Closing stale PR #$PR_NUMBER ($OLD_BRANCH)"
      curl -s --fail-with-body -X PATCH "${FORGEJO_SERVER_URL}/api/v1/repos/${FORGEJO_REPOSITORY}/pulls/$PR_NUMBER" \
        -H "Authorization: token ${FORGEJO_TOKEN}" \
        -H "Content-Type: application/json" \
        -d '{"state": "closed"}'
    fi
    echo "Deleting stale branch $OLD_BRANCH"
    curl -s --fail-with-body -X DELETE "${FORGEJO_SERVER_URL}/api/v1/repos/${FORGEJO_REPOSITORY}/branches/$OLD_BRANCH" \
      -H "Authorization: token ${FORGEJO_TOKEN}"
  done

  git checkout -b "$BRANCH"
  nix fmt -- "$PKG_DIR"
  git add "$PKG_DIR"
  git commit -m "pkgs/$PKG: $CURRENT -> $LATEST"
  git push origin "$BRANCH"

  curl -s --fail-with-body -X POST "${FORGEJO_SERVER_URL}/api/v1/repos/${FORGEJO_REPOSITORY}/pulls" \
    -H "Authorization: token ${FORGEJO_TOKEN}" \
    -H "Content-Type: application/json" \
    -d "{
      \"title\": \"pkgs/$PKG: $CURRENT -> $LATEST\",
      \"head\": \"$BRANCH\",
      \"base\": \"master\",
      \"body\": \"Automated package update.\"
    }"

  git checkout master
  echo "::endgroup::"
done
