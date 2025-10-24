FROM mcr.microsoft.com/dotnet/sdk:9.0

RUN mkdir -p /home/app

COPY ./bin/Release/net9.0/publish /home/app

WORKDIR /home/app

CMD ["dotnet", "Account.Service.dll"]
