{
  lib,
  writeShellScriptBin,
  jq,
  gitMinimal,
  gawk,
  coreutils,
}:

# Claude Code statusline: a vendored POSIX shell script (statusline-command.sh)
# wrapped so its runtime deps resolve regardless of the ambient PATH.
writeShellScriptBin "claude-statusline" (
  ''
    export PATH=${
      lib.makeBinPath [
        jq
        gitMinimal
        gawk
        coreutils
      ]
    }:"$PATH"
  ''
  + builtins.readFile ./statusline-command.sh
)
