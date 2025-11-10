**Aims to be the swiss army knife for your business banking operations.**

### Key Technologies
1. Akka.NET: [actor model](https://getakka.net/articles/concepts/actor-systems.html), [event sourcing](https://getakka.net/articles/persistence/architecture.html), [cluster sharded entities](https://getakka.net/articles/clustering/cluster-sharding.html)
2. PostgreSQL
3. [React via F# Feliz](https://fable-hub.github.io/Feliz/ecosystem/Hooks/Feliz.UseElmish)
4. RabbitMQ: Queued communication with external services
5. [SignalR](https://dotnet.microsoft.com/en-us/apps/aspnet/signalr): Provides feedback from actors running on the server to components in the React SPA
6. [Quartz.NET](https://www.quartz-scheduler.net/): Future/recurring actor message scheduling with PostgreSQL persistence
7. [Pulumi](https://www.pulumi.com): IaC via TypeScript for managing Kubernetes resources locally or deployed to Azure Kubernetes Service

### Use Cases
1. Automatic Balance Management
   - Maintain a zero balance after each transaction
   - Maintain a daily target balance by allowing a partner account to absorb excess cash or replenish cash to the target account
   - Distribute balance among multiple accounts within an org on a per-transaction, daily, or twice-monthly schedule
2. [Lithic](https://www.lithic.com) card issuer makes requests to our platform to authorize employee debit card purchases & communicate progression of the purchase lifecycle
3. Approval Rules
   - Require admin approval for inviting employees, unlocking cards, fulfilling payments, etc.
   - Restrict submission of actions by type, daily accrued transaction amount, and transaction amount with upper and lower bounds (ex: amounts less than $1500 require 1 approvers; amounts between $1500 and $10,000 require 2 approvers)
   - Assign multiple approvers per rule (ex: Den Vau, Pop Pongkool, & Any Admin = 3 approvals required to submit an action associated with this rule)
4. Payment requests between organizations on the platform or external entities
   - Optional recurring schedule (ex: due date every 3 months on the last Monday of the month until 4 payments made)
   - Optional invoice line-item breakdown
5. Transfer money immediately or on a future date to other organizations on the platform or domestically (ex: ACH)
6. Partner Bank
   - Create a legal entity for the business during onboarding
   - Process domestic transfers (ex: ACH)
   - Currently set up as a separate server processing TCP requests from the platform, mocking interactions between platform and partner bank
   - TODO: Integrate with an actual partner bank ([column](https://column.com))
7. Recurring maintenance fee for each billing cycle unless a qualified deposit found or a daily account balance threshold met.
8. Manage daily/monthly employee purchase limits per employee card
9. Monitor employee purchases and other activities on the platform
10. Billing statements issued for each billing cycle
11. Emails sent for payment requests, employee invites, purchase declined, etc.

### Demonstration
#### Automatic Balance Management
![auto-balance-management](https://github.com/user-attachments/assets/7fdeaaf8-e255-4d64-a9ff-33addf9f9fa5)

#### Recurring Payments
![payment-recurring](https://github.com/user-attachments/assets/07681a74-cdb5-4112-a9f2-7b49858f0ce9)

#### Analytics
![analytics](https://github.com/user-attachments/assets/a68249dd-8c6d-4473-a08e-3f9814400664)

#### Approvals
![approvals](https://github.com/user-attachments/assets/e81135c5-bc2a-4e6c-9c8d-00a3f832f792)

#### Transactions
![transactions](https://github.com/user-attachments/assets/70ac0809-03f5-4868-9e3f-7aca0748f222)

### Running with Kubernetes via minikube
1. Dependencies: [.NET 9](https://dotnet.microsoft.com/en-us/download), [minikube](https://minikube.sigs.k8s.io/docs/start/), [pulumi](https://www.pulumi.com/docs/install/), [Node.js 18](https://docs.npmjs.com/downloading-and-installing-node-js-and-npm)
2. `sh build.sh -t RunK8sApp`
3. Browser opens automatically after all K8s resources start up
4. Enable postgres port forwarding if you want to inspect postgres in a local client: `sh postgres-port-forward-k8s.sh` (Server=postgres;Database=akkabank;Uid=testuser;Pwd=testpass)
5. View Akka cluster and actor info via [Petabridge.Cmd](https://cmd.petabridge.com/articles/commands/cluster-commands.html):
   ```
   > minikube kubectl -- exec --stdin --tty account-cluster-0 -- /bin/bash
   > dotnet tool run pbm
   > cluster show
   > actor hierarchy
   ```

### Running with Docker
1. Dependencies: [.NET 9](https://dotnet.microsoft.com/en-us/download), [Node.js 18](https://docs.npmjs.com/downloading-and-installing-node-js-and-npm)
2. `sh build.sh -t RunDockerApp`
3. Navigate to localhost:3000
4. If you want to inspect postgres in a dashboard you can visit localhost:5008 (Server=postgres;Database=akkabank;Uid=postgres;Pwd=password)

### Running without Docker or K8s
1. Dependencies: [.NET 9](https://dotnet.microsoft.com/en-us/download), [Node.js 18](https://docs.npmjs.com/downloading-and-installing-node-js-and-npm), [PostgreSQL & psql](https://www.postgresql.org/download/)
2. Create a database (Server=localhost;Database=akkabank;Uid=postgres;Pwd=password)
3. Seed the database: `psql -h 127.0.0.1 -p 5432 -U postgres akkabank < Database/Init/Bank.sql`
4. `cd` into ./Web, ./Account/Service, ./Scheduler/Service, & ./MockPartnerBank.Service & `dotnet run` in each
5. `cd` into ./UI and `npm run build` or `npm start`

### Deploying to Azure AKS
1. Dependencies: [.NET 9](https://dotnet.microsoft.com/en-us/download), [Azure CLI](https://learn.microsoft.com/en-us/cli/azure/install-azure-cli), [Pulumi](https://www.pulumi.com/docs/install/), [Node.js 18](https://docs.npmjs.com/downloading-and-installing-node-js-and-npm)
2. `sh build.sh -t DeployAllStaging` (You will be prompted to sign in to Pulumi & Azure CLI)
3. One pulumi stack of Azure AKS resources and another Pulumi stack for K8s resources will be created.  A [Pulumi ESC](https://www.pulumi.com/product/esc/) staging environment will be created and app environment configuration will be set.  Your local kubeconfig file will be modified to include details needed to connect to the AKS cluster.  See `kubectl get all --namespace akkabank` & `kubectl get all --namespace app-routing-system`.  App images are currently being pulled from my public docker hub repos.  An IP will be exported to the console when Pulumi finishes creating resources.

### Running tests
1. `sh build.sh -t Test`

### Archive
This project was inspired from reading [Functional Programming in C#](https://www.manning.com/books/functional-programming-in-c-sharp-second-edition)
by **Enrico Buonanno**.  

The first iteration of this project [(see CSharpWithLanguageExt directory)](https://github.com/danne931/akka-dotnet-bank/tree/main/Archive/CSharpWithLanguageExt) expands on **Enrico Buonanno**'s banking account example [actor](https://github.com/la-yumba/functional-csharp-code-2/blob/master/Examples/Chapter19/Boc/AccountProcess.cs) to include
additional business use cases as well as integration with [EventStoreDB](https://www.eventstore.com/eventstoredb) and C#'s functional programming toolkit, [language-ext](https://github.com/louthy/language-ext).

The second iteration [(see FSharpWithAkka directory)](https://github.com/danne931/akka-dotnet-bank/tree/main/Archive/FSharpWithAkka) is close to a one-to-one representation of the CSharpWithLanguageExt directory, with all use cases rewritten in F#.  I saw that [F#'s type inference](https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/type-inference), [computation expressions](https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/computation-expressions) and immutable data structures made writing programs with typed functional programming more second nature than with C# so I decided to continue with it for the final iteration.
