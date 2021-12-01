FROM mcr.microsoft.com/dotnet/runtime:6.0 AS base
WORKDIR /app

FROM mcr.microsoft.com/dotnet/sdk:6.0 AS build
WORKDIR /src
COPY ["Day1/Day1.fsproj", "Day1/"]
RUN dotnet restore "Day1/Day1.fsproj"
COPY . .
WORKDIR "/src/Day1"
RUN dotnet build "Day1.fsproj" -c Release -o /app/build

FROM build AS publish
RUN dotnet publish "Day1.fsproj" -c Release -o /app/publish

FROM base AS final
WORKDIR /app
COPY --from=publish /app/publish .
ENTRYPOINT ["dotnet", "Day1.dll"]
