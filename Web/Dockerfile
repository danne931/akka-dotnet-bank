FROM mcr.microsoft.com/dotnet/aspnet:9.0

RUN mkdir -p /home/app

COPY ./bin/Release/net9.0/publish /home/app

WORKDIR /home/app

CMD ["dotnet", "Web.dll"]
