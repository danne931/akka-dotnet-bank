FROM bitnami/dotnet-sdk:9

RUN mkdir -p /home/app

COPY ./bin/Release/net9.0/publish /home/app

WORKDIR /home/app

CMD ["dotnet", "MockPartnerBank.Service.dll"]
