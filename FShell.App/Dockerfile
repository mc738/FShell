FROM mcr.microsoft.com/dotnet/runtime:6.0 AS base
WORKDIR /app

FROM mcr.microsoft.com/dotnet/sdk:6.0 AS build
WORKDIR /src
COPY ["FShell.App/FShell.App.fsproj", "FShell.App/"]
RUN dotnet restore "FShell.App/FShell.App.fsproj"
COPY . .
WORKDIR "/src/FShell.App"
RUN dotnet build "FShell.App.fsproj" -c Release -o /app/build

FROM build AS publish
RUN dotnet publish "FShell.App.fsproj" -c Release -o /app/publish

FROM base AS final
WORKDIR /app
COPY --from=publish /app/publish .
ENTRYPOINT ["dotnet", "FShell.App.dll"]
