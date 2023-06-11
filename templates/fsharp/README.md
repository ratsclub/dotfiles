# F# Project Template

## Getting stated with this template

1. create a solution: `dotnet new sln --name <solution-name>`
2. add the `build` project to the solution `dotnet sln add build`
3. create a project on the `src/` directory: `dotnet new console -lang F# --output "src/<project-name>"`
4. create a test project on the `tests/` directory: `dotnet new console -lang F# --output "tests/<project-name>"`
5. add both to your project to the solution: `dotnet sln add src/<project-name> && dotnet sln add tests/<project-name>`

## How to run

1. Install the tools: `dotnet restore tools`
2. Restore: `dotnet paket restore`
3. Check the available targets: `dotnet run --project build -- --list`
4. Run the desired target. ex: `dotnet run --project build -- build`
