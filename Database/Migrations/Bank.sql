ALTER DATABASE akkabank SET TIMEZONE TO 'UTC';

CREATE EXTENSION IF NOT EXISTS pg_trgm;
CREATE EXTENSION IF NOT EXISTS pg_stat_statements;

ALTER SYSTEM SET shared_preload_libraries = 'pg_stat_statements';
SELECT pg_reload_conf();

begin;

DROP VIEW IF EXISTS daily_purchase_accrued;
DROP VIEW IF EXISTS monthly_purchase_accrued;
DROP VIEW IF EXISTS daily_purchase_accrued_by_card;
DROP VIEW IF EXISTS monthly_purchase_accrued_by_card;

DROP TABLE IF EXISTS balance_history;
DROP TABLE IF EXISTS billingstatement;
DROP TABLE IF EXISTS payment_platform;
DROP TABLE IF EXISTS payment_third_party;
DROP TABLE IF EXISTS payment;
DROP TABLE IF EXISTS transfer_internal;
DROP TABLE IF EXISTS transfer_domestic;
DROP TABLE IF EXISTS transfer;
DROP TABLE IF EXISTS transfer_domestic_recipient;
DROP TABLE IF EXISTS merchant;
DROP TABLE IF EXISTS ancillarytransactioninfo;
DROP TABLE IF EXISTS account_event;
DROP TABLE IF EXISTS card;
DROP TABLE IF EXISTS command_approval_progress;
DROP TABLE IF EXISTS command_approval_rule_amount_daily_limit;
DROP TABLE IF EXISTS command_approval_rule_amount_per_command;
DROP TABLE IF EXISTS command_approval_rule;
DROP TABLE IF EXISTS org_feature_flag;
DROP TABLE IF EXISTS account;
DROP TABLE IF EXISTS employee_event;
DROP TABLE IF EXISTS organization_event;
DROP TABLE IF EXISTS employee;
DROP TABLE IF EXISTS organization;
DROP TABLE IF EXISTS category;

DROP TYPE IF EXISTS money_flow CASCADE;
DROP TYPE IF EXISTS monthly_time_series_filter_by CASCADE;
DROP TYPE IF EXISTS time_frame CASCADE;
DROP TYPE IF EXISTS employee_status;
DROP TYPE IF EXISTS organization_status;
DROP TYPE IF EXISTS employee_role;
DROP TYPE IF EXISTS account_depository;
DROP TYPE IF EXISTS account_status;
DROP TYPE IF EXISTS auto_transfer_rule_frequency;
DROP TYPE IF EXISTS card_status;
DROP TYPE IF EXISTS card_type;
DROP TYPE IF EXISTS platform_payment_status;
DROP TYPE IF EXISTS third_party_payment_status;
DROP TYPE IF EXISTS payment_type;
DROP TYPE IF EXISTS payment_network;
DROP TYPE IF EXISTS domestic_transfer_recipient_account_depository;
DROP TYPE IF EXISTS domestic_transfer_recipient_status;
DROP TYPE IF EXISTS domestic_transfer_status;
DROP TYPE IF EXISTS internal_transfer_status;
DROP TYPE IF EXISTS transfer_category;
DROP TYPE IF EXISTS approvable_command;
DROP TYPE IF EXISTS command_approval_criteria;
DROP TYPE IF EXISTS command_approval_status;

-- Drop Akka event sourcing tables.
-- These tables are initiated in Infrastructure/Akka.fs.
-- They are created automatically when starting up the app so no need
-- create those tables here.
DROP TABLE IF EXISTS tags;
DROP TABLE IF EXISTS akka_snapshots;
DROP TABLE IF EXISTS akka_event_journal;


--- UTILITY FUNCTIONS ---
CREATE OR REPLACE FUNCTION raise_update_not_allowed()
RETURNS trigger AS $$
BEGIN
   RAISE EXCEPTION 'UPDATE is not allowed on table: %', TG_TABLE_NAME;
   RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION prevent_update(table_name text)
RETURNS void AS $$
BEGIN
   EXECUTE format('
      CREATE TRIGGER prevent_update
      BEFORE UPDATE ON %I
      FOR EACH STATEMENT
      EXECUTE FUNCTION raise_update_not_allowed();',
      table_name
   );
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION add_created_at_column(table_name text)
RETURNS void AS $$
BEGIN
   EXECUTE format(
      'ALTER TABLE %I ADD COLUMN IF NOT EXISTS created_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP;',
      table_name
   );
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION update_timestamp()
RETURNS trigger AS $$
BEGIN
   NEW.updated_at = NOW();
   RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION add_updated_at_column_and_trigger(table_name text)
RETURNS void AS $$
BEGIN
   EXECUTE format(
      'ALTER TABLE %I ADD COLUMN IF NOT EXISTS updated_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP;',
      table_name
   );

   EXECUTE format('
      CREATE TRIGGER set_timestamp_%I
      BEFORE UPDATE ON %I
      FOR EACH ROW
      EXECUTE FUNCTION update_timestamp();',
      table_name, table_name
   );
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION add_soft_delete_column(table_name text)
RETURNS void AS $$
BEGIN
   EXECUTE format(
      'ALTER TABLE %I ADD COLUMN IF NOT EXISTS deleted_at TIMESTAMPTZ;',
      table_name
   );
END;
$$ LANGUAGE plpgsql;

CREATE TYPE organization_status AS ENUM (
  'PendingOnboardingTasksFulfilled',
  'Active'
);

--- ORGANIZATION ---
CREATE TABLE organization (
   org_id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
   org_name VARCHAR(100) UNIQUE NOT NULL,
   status organization_status NOT NULL,
   status_detail JSONB NOT NULL
);

SELECT add_created_at_column('organization');
SELECT add_updated_at_column_and_trigger('organization');

CREATE INDEX org_org_id_idx ON organization(org_id);
CREATE INDEX org_search_idx ON organization USING gist (org_name gist_trgm_ops);


--- ACCOUNT ---
CREATE TYPE account_depository AS ENUM ('Checking', 'Savings');
CREATE TYPE account_status AS ENUM ('Pending', 'Active', 'Closed', 'ReadyForDelete');
CREATE TYPE auto_transfer_rule_frequency AS ENUM ('PerTransaction', 'Daily', 'TwiceMonthly');

CREATE TABLE account (
   account_id UUID PRIMARY KEY,
   routing_number INT NOT NULL,
   account_number BIGINT UNIQUE NOT NULL,
   account_name VARCHAR(50) NOT NULL,
   depository account_depository NOT NULL,
   balance MONEY NOT NULL,
   currency VARCHAR(3) NOT NULL,
   status account_status NOT NULL,
   auto_transfer_rule JSONB,
   auto_transfer_rule_frequency auto_transfer_rule_frequency,
   maintenance_fee_qualifying_deposit_found BOOLEAN NOT NULL,
   maintenance_fee_daily_balance_threshold BOOLEAN NOT NULL,
   org_id UUID NOT NULL REFERENCES organization,
   last_billing_cycle_at TIMESTAMPTZ
);

SELECT add_created_at_column('account');
SELECT add_updated_at_column_and_trigger('account');

CREATE INDEX account_last_billing_cycle_at_idx ON account(last_billing_cycle_at);
CREATE INDEX account_org_id_idx ON account(org_id);

COMMENT ON TABLE account IS
'Checking and savings accounts on the platform.

TODO: Research potential for "treasury accounts".
TODO: Research potential for "credit accounts".
TODO: Create a separate table for "LinkedAccounts".  A linked account should refer to an account outside 
the platform, a Chase bank account for example, which the organization can set up (likely via Plaid), 
to be able to deposit funds into their accounts on the platform.';

COMMENT ON COLUMN account.auto_transfer_rule IS
'This optional configuration allows an organization to automatically
manage balances of accounts in their organization.
Rule configuration types to choose from include ZeroBalance, TargetBalance and PercentDistribution.

ZeroBalance:
Move 100% of account balance to an account within the org after each transaction.

TargetBalance:
Maintain a target balance, or some range around a balance, on a daily basis.
The account this rule is applied to will interact with an additional "partner account"
which we may view as a "BiDirectionalTransferContact".  By "bidirectional", we mean to
say that this partner account will absorb excess cash from the account or replenish cash
to the account in order to maintain the target balance.

PercentDistribution:
Allocate 100% of account balance split among accounts within an org.
The allocation schedule can be configured to occur for every transaction,
on a daily basis, or on a twice monthly basis.';

COMMENT ON COLUMN account.auto_transfer_rule_frequency IS
'How often to check whether an account should
manage their balance by automatically transferring funds.';

COMMENT ON COLUMN account.last_billing_cycle_at IS
'Used by BillingCycleActor to determine which accounts to send a
StartBillingCycleCommand to.';


--- ORG FEATURE FLAGS ---
CREATE TABLE org_feature_flag (
   social_transfer_discovery_account_id UUID REFERENCES account,
   org_id UUID NOT NULL UNIQUE REFERENCES organization,
   id UUID PRIMARY KEY DEFAULT gen_random_uuid()
);

SELECT add_created_at_column('org_feature_flag');
SELECT add_updated_at_column_and_trigger('org_feature_flag');

CREATE INDEX org_feature_flag_social_transfer_discovery_account_id_idx
ON org_feature_flag (social_transfer_discovery_account_id)
WHERE social_transfer_discovery_account_id IS NOT NULL;

COMMENT ON COLUMN org_feature_flag.social_transfer_discovery_account_id IS
'An organization can choose to make one of their accounts discoverable by other
orgs on the platform.  If set, other orgs will be able to search for this organization
by name/email and then transfer money to the configured "social transfer discovery account"
for this org.';


--- BILLING STATEMENTS ---
CREATE TABLE billingstatement (
   name VARCHAR(100) NOT NULL,
   account_id UUID NOT NULL REFERENCES account ON DELETE CASCADE,
   last_persisted_event_sequence_number BIGINT NOT NULL,
   transactions JSONB NOT NULL,
   balance MONEY NOT NULL,
   month INT NOT NULL,
   year INT NOT NULL,
   account_snapshot BYTEA NOT NULL,
   id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
   org_id UUID NOT NULL REFERENCES organization
);

SELECT add_created_at_column('billingstatement');
SELECT prevent_update('billingstatement');

CREATE INDEX billingstatement_org_id_idx ON billingstatement(org_id);
CREATE INDEX billingstatement_account_id_idx ON billingstatement(account_id);


--- CATEGORIES ---
CREATE TABLE category (
   category_id SMALLSERIAL PRIMARY KEY,
   name VARCHAR(100) UNIQUE NOT NULL
);

SELECT add_created_at_column('category');
SELECT add_updated_at_column_and_trigger('category');

COMMENT ON TABLE category IS
'Typically represents a purchase-related category.

Allows users to filter transactions by predefined categories such as
Restaurants, Taxis, Office Supplies, etc.
NOTE: No implementation of custom category creation at this time.';

--- MERCHANTS ---
CREATE TABLE merchant (
   org_id UUID NOT NULL REFERENCES organization,
   name VARCHAR(100) NOT NULL,
   alias VARCHAR(100),

   PRIMARY KEY (org_id, name)
);

SELECT add_created_at_column('merchant');
SELECT add_updated_at_column_and_trigger('merchant');

COMMENT ON TABLE merchant IS
'Allows an organization to control how transaction merchant info is displayed across the app.

If configured, the alias field will be displayed for past and future transactions for
all users.  For example, maybe the merchant info comes across our network from the
payment processor as something vague.  In this situation we would want to 
provide an alias to increase clarity for the user experience.';


--- EMPLOYEES ---
CREATE TYPE employee_status AS ENUM (
  'PendingInviteConfirmation',
  'PendingInviteApproval',
  'PendingRestoreAccessApproval',
  'Active',
  'Closed',
  'ReadyForDelete'
);
CREATE TYPE employee_role AS ENUM ('Admin', 'Scholar', 'CardOnly');

CREATE TABLE employee (
   employee_id UUID PRIMARY KEY,
   email VARCHAR(255) UNIQUE NOT NULL,
   first_name VARCHAR(50) NOT NULL,
   last_name VARCHAR(50) NOT NULL,
   search_query TEXT,
   role employee_role NOT NULL,
   status employee_status NOT NULL,
   status_detail JSONB NOT NULL,
   pending_purchases JSONB NOT NULL,
   onboarding_tasks JSONB NOT NULL,
   cards JSONB NOT NULL,
   invite_token UUID,
   invite_expiration TIMESTAMPTZ,
   auth_provider_user_id UUID,
   org_id UUID NOT NULL REFERENCES organization
);

SELECT add_created_at_column('employee');
SELECT add_updated_at_column_and_trigger('employee');

CREATE INDEX employee_email_idx ON employee(email);
CREATE INDEX employee_invite_token_idx ON employee(invite_token);
CREATE INDEX employee_search_query_idx ON employee USING gist (search_query gist_trgm_ops);
CREATE INDEX employee_org_id_idx ON employee(org_id);

CREATE OR REPLACE FUNCTION update_search_query()
RETURNS trigger AS $$
BEGIN
  NEW.search_query = concat_ws(' ', NEW.first_name, NEW.last_name, NEW.email);
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER update_search_query_trigger
BEFORE INSERT OR UPDATE ON employee
FOR EACH ROW
EXECUTE FUNCTION update_search_query();

COMMENT ON COLUMN employee.search_query IS
'search_query exists for text searches on employee name/email
(see update_search_query_trigger above).  Being able to search by employee name/email
comes in handy in dashboard forms and filtering transaction, history, or
employees in the UI.';

COMMENT ON COLUMN employee.onboarding_tasks IS
'An employee may optionally be configured with onboarding tasks when they are
created by a team member of an organization on the platform.
However, we do not want these onboarding tasks to execute until the employee
confirms their invite via email.
Currently, only one onboarding task type is available and that is to create a virtual
debit card for the employee.  So the onboarding task "CreateCard" is essentially a bit of
config info provided at employee creation time such as the AccountId to link the card to,
purchase limits, and name which will be enough to initiate the card creation if the employee
confirms the email invitation.';


--- ORG EVENTS ---
CREATE TABLE organization_event (
   name VARCHAR(50) NOT NULL,
   timestamp TIMESTAMPTZ NOT NULL,
   event_id UUID PRIMARY KEY,
   correlation_id UUID NOT NULL,
   initiated_by_id UUID NOT NULL REFERENCES employee(employee_id),
   event JSONB NOT NULL,
   org_id UUID NOT NULL REFERENCES organization
);

SELECT add_created_at_column('organization_event');
SELECT prevent_update('organization_event');

CREATE INDEX organization_event_org_id_idx ON organization_event(org_id);
CREATE INDEX organization_event_org_id_timestamp_idx ON organization_event(org_id, timestamp desc);
CREATE INDEX organization_event_initiated_by_id_idx ON organization_event(initiated_by_id);
CREATE INDEX organization_event_correlation_id_idx ON organization_event(correlation_id);

COMMENT ON TABLE organization_event IS
'Read model representation of Akka event sourced organization events.';
COMMENT ON COLUMN organization_event.event IS
'Representation of the organization_event in its Akka event sourcing form.';


--- EMPLOYEE EVENTS ---
CREATE TABLE employee_event (
   name VARCHAR(50) NOT NULL,
   timestamp TIMESTAMPTZ NOT NULL,
   event_id UUID PRIMARY KEY,
   employee_id UUID NOT NULL REFERENCES employee,
   correlation_id UUID NOT NULL,
   initiated_by_id UUID NOT NULL REFERENCES employee(employee_id),
   event JSONB NOT NULL,
   org_id UUID NOT NULL REFERENCES organization
);

SELECT add_created_at_column('employee_event');
SELECT prevent_update('employee_event');

CREATE INDEX employee_event_org_id_timestamp_idx ON employee_event(org_id, timestamp desc);
CREATE INDEX employee_event_timestamp_brin ON employee_event USING BRIN(timestamp);
CREATE INDEX employee_event_employee_id_idx ON employee_event(employee_id);
CREATE INDEX employee_event_initiated_by_id_idx ON employee_event(initiated_by_id);
CREATE INDEX employee_event_correlation_id_idx ON employee_event(correlation_id);

COMMENT ON TABLE employee_event IS
'Read model representation of Akka event sourced employee events.

These events are not necessarily tied to account events.
Ex:
Creating a debit card, applying purchase limits to a card, locking a card,
updating employee role, etc.';
COMMENT ON COLUMN employee_event.event IS
'Representation of the employee_event in its Akka event sourcing form.';


--- CARDS ---
CREATE TYPE card_status AS ENUM ('Active', 'Frozen', 'Closed');
CREATE TYPE card_type AS ENUM ('Debit', 'Credit');

CREATE TABLE card (
   card_number_last_4 VARCHAR(4) NOT NULL,
   daily_purchase_limit MONEY NOT NULL,
   monthly_purchase_limit MONEY NOT NULL,
   virtual BOOLEAN NOT NULL,
   card_status card_status NOT NULL,
   card_type card_type NOT NULL,
   card_nickname VARCHAR(50),
   last_purchase_at TIMESTAMPTZ,
   exp_month INT NOT NULL,
   exp_year INT NOT NULL,
   card_id UUID PRIMARY KEY,
   employee_id UUID NOT NULL REFERENCES employee,
   account_id UUID NOT NULL REFERENCES account,
   org_id UUID NOT NULL REFERENCES organization
);

SELECT add_created_at_column('card');
SELECT add_updated_at_column_and_trigger('card');

CREATE INDEX card_org_id_idx ON card(org_id);
CREATE INDEX card_account_id_idx ON card(account_id);
CREATE INDEX card_employee_id_idx ON card(employee_id);

COMMENT ON COLUMN card.account_id IS
'Every card must be linked to an account within an organization.
When an employee makes a purchase with their card, funds will be
deducted from the organization account the card is linked to.';


--- ACCOUNT EVENTS ---
CREATE TYPE money_flow AS ENUM ('In', 'Out');

CREATE TABLE account_event (
   name VARCHAR(50) NOT NULL,
   amount MONEY,
   money_flow money_flow,
   source VARCHAR(100),
   timestamp TIMESTAMPTZ NOT NULL,
   event_id UUID PRIMARY KEY,
   account_id UUID NOT NULL REFERENCES account ON DELETE CASCADE,
   initiated_by_id UUID NOT NULL REFERENCES employee(employee_id),
   correlation_id UUID NOT NULL,
   card_id UUID REFERENCES card,
   event JSONB NOT NULL,
   org_id UUID NOT NULL REFERENCES organization
);

SELECT add_created_at_column('account_event');
SELECT prevent_update('account_event');

CREATE INDEX account_event_account_id_idx ON account_event(account_id);
CREATE INDEX account_event_initiated_by_id_idx ON account_event(initiated_by_id);
CREATE INDEX account_event_card_id_idx ON account_event(card_id);
CREATE INDEX account_event_org_id_timestamp_idx ON account_event(org_id, timestamp desc);
CREATE INDEX account_event_timestamp_brin ON account_event USING BRIN(timestamp);
CREATE INDEX account_event_accrued_amount_view_query_idx ON account_event(amount, name, timestamp);
CREATE INDEX account_event_correlation_id_idx ON account_event(correlation_id);

COMMENT ON TABLE account_event IS
'account_event is the read model representation of Akka event sourced account events.';
COMMENT ON COLUMN account_event.event IS
'Representation of the account_event in its Akka event sourcing form.';
COMMENT ON COLUMN account_event.source IS
'Source may be a merchant name, transfer recipient name, etc. depending on the account_event type.
This property is used only for analytics queries.';
COMMENT ON COLUMN account_event.correlation_id IS
'Correlation ID allows us to trace the lifecycle of some transaction
(ex: DomesticTransferRequested -> DomesticTransferProgressUpdate -> DomesticTransferCompleted)
is an example where 3 events share the same correlation_id.';

--- ANCILLARY TRANSACTION INFO ---
CREATE TABLE ancillarytransactioninfo (
   note TEXT,
   category_id SMALLSERIAL REFERENCES category,
   transaction_id UUID PRIMARY KEY
);

SELECT add_created_at_column('ancillarytransactioninfo');
SELECT add_updated_at_column_and_trigger('ancillarytransactioninfo');

CREATE INDEX ancillarytransactioninfo_category_id_idx ON ancillarytransactioninfo(category_id) WHERE category_id IS NOT NULL;

ALTER TABLE ancillarytransactioninfo
ALTER COLUMN category_id DROP NOT NULL;

COMMENT ON TABLE ancillarytransactioninfo IS
'A user may provide supporting info for a transaction such as a note or a category
(see category table).

Ancillary transaction info is inserted lazily when the user first provides
this supporting info within the transaction detail component.
TODO: Consider how we might attempt to automatically categorize incoming transactions so
organizations would not have to apply these categories by hand.';

COMMENT ON COLUMN ancillarytransactioninfo.transaction_id IS
'transaction_id refers to the correlation_id of a transaction table record.';


--- BALANCE HISTORY ---
CREATE TABLE balance_history(
   account_id UUID NOT NULL REFERENCES account ON DELETE CASCADE,
   date DATE NOT NULL,
   balance NUMERIC NOT NULL,
   id SERIAL PRIMARY KEY,
   CONSTRAINT account_date UNIQUE (account_id, date)
);

SELECT add_created_at_column('balance_history');
SELECT prevent_update('balance_history');

COMMENT ON TABLE balance_history IS
'Represents the end-of-day balance for an account.

A balance history record is inserted every day for every account.
A newly inserted record represents the balance for the previous day.
See update_balance_history_for_yesterday procedure.';


--- TRANSFERS ---
CREATE TYPE transfer_category AS ENUM (
  'InternalWithinOrg',
  'InternalAutomatedWithinOrg',
  'InternalBetweenOrgs',
  'Domestic'
);

CREATE TABLE transfer(
   transfer_id UUID PRIMARY KEY,
   initiated_by_id UUID NOT NULL REFERENCES employee(employee_id),
   amount MONEY NOT NULL,
   transfer_category transfer_category NOT NULL,
   scheduled_at TIMESTAMPTZ NOT NULL,
   sender_org_id UUID NOT NULL REFERENCES organization(org_id),
   sender_account_id UUID NOT NULL REFERENCES account(account_id),
   memo TEXT
);

SELECT add_created_at_column('transfer');
SELECT add_updated_at_column_and_trigger('transfer');

CREATE INDEX transfer_initiated_by_id_idx ON transfer (initiated_by_id);
CREATE INDEX transfer_sender_account_id_idx ON transfer (sender_account_id);
CREATE INDEX transfer_sender_org_id_idx ON transfer (sender_org_id);

COMMENT ON TABLE transfer IS
'Parent of two kinds of transfers, each of which has an associated child table:

- Internal transfers (within the platform)
- Domestic transfers (from an account on the platform to a domestic account outside the platform)
';

--- INTERNAL TRANSFERS ---
CREATE TYPE internal_transfer_status AS ENUM (
   'Scheduled',
   'Pending',
   'Completed',
   'Deposited',
   'Failed'
);

CREATE TABLE transfer_internal(
   transfer_id UUID PRIMARY KEY REFERENCES transfer,
   transfer_status internal_transfer_status NOT NULL,
   transfer_status_detail JSONB NOT NULL,
   recipient_org_id UUID NOT NULL REFERENCES organization(org_id),
   recipient_account_id UUID REFERENCES account(account_id)
);

SELECT add_created_at_column('transfer_internal');
SELECT add_updated_at_column_and_trigger('transfer_internal');

CREATE INDEX transfer_internal_recipient_org_id_idx ON transfer_internal (recipient_org_id);
CREATE INDEX transfer_internal_recipient_account_id_idx ON transfer_internal (recipient_account_id)
WHERE recipient_account_id IS NOT NULL;

COMMENT ON TABLE transfer_internal IS
'Child table of transfers represents transfers which occur within the platform.

Transfers occur either between accounts within an organization or between organizations.';

--- DOMESTIC TRANSFER RECIPIENTS ---
CREATE TYPE domestic_transfer_recipient_account_depository
AS ENUM ('Checking', 'Savings'); 

CREATE TYPE domestic_transfer_recipient_status
AS ENUM ('Confirmed', 'InvalidAccount', 'Closed');

CREATE TYPE payment_network AS ENUM ('ACH');

CREATE TABLE transfer_domestic_recipient(
   recipient_account_id UUID PRIMARY KEY,
   sender_org_id UUID REFERENCES organization(org_id),
   first_name VARCHAR(50) NOT NULL,
   last_name VARCHAR(50) NOT NULL,
   nickname VARCHAR(100),
   routing_number INT NOT NULL,
   account_number BIGINT UNIQUE NOT NULL,
   recipient_status domestic_transfer_recipient_status NOT NULL,
   depository domestic_transfer_recipient_account_depository NOT NULL,
   payment_network payment_network NOT NULL
);

SELECT add_created_at_column('transfer_domestic_recipient');
SELECT add_updated_at_column_and_trigger('transfer_domestic_recipient');

CREATE INDEX transfer_domestic_recipient_sender_org_id_idx ON transfer_domestic_recipient(sender_org_id);


--- DOMESTIC TRANSFERS ---
CREATE TYPE domestic_transfer_status AS ENUM (
   'Scheduled',
   'Outgoing',
   'InProgress',
   'Completed',
   'Failed'
);

CREATE TABLE transfer_domestic(
   transfer_id UUID PRIMARY KEY REFERENCES transfer,
   transfer_status domestic_transfer_status NOT NULL,
   transfer_status_detail JSONB NOT NULL,
   recipient_account_id UUID REFERENCES transfer_domestic_recipient
);

SELECT add_created_at_column('transfer_domestic');
SELECT add_updated_at_column_and_trigger('transfer_domestic');

CREATE INDEX transfer_domestic_recipient_account_id_idx ON transfer_domestic (recipient_account_id)
WHERE recipient_account_id IS NOT NULL;

COMMENT ON TABLE transfer_domestic IS
'Child table of transfers represents transfers which occur outside the platform, domestically within the U.S.

There is currently a server, defined in MockDomesticTransferProcessor.Web folder, which supports mocking
the interaction of sending transfers domestically from accounts on the platform to accounts outside the platform.
Transfer request and progress check messages are sent over TCP from the platform (Transfer.App/DomesticTransferRecipientActor.fs)
to a mock 3rd party transfer processor (MockDomesticTransferProcess.Web/Program.fs).
TODO: Look into integrating Plaid.';


--- PAYMENTS ---
CREATE TYPE payment_type AS ENUM ('Platform', 'ThirdParty');

CREATE TABLE payment(
   payment_id UUID PRIMARY KEY,
   initiated_by_id UUID NOT NULL REFERENCES employee(employee_id),
   amount MONEY NOT NULL,
   memo TEXT NOT NULL,
   payment_type payment_type NOT NULL,
   expiration TIMESTAMPTZ NOT NULL,
   payee_org_id UUID NOT NULL REFERENCES organization(org_id),
   payee_account_id UUID NOT NULL REFERENCES account(account_id)
);

SELECT add_created_at_column('payment');
SELECT add_updated_at_column_and_trigger('payment');

CREATE INDEX payment_payee_org_id_idx ON payment(payee_org_id);
CREATE INDEX payment_payee_account_id_idx ON payment(payee_account_id);
CREATE INDEX payment_initiated_by_id_idx ON payment(initiated_by_id);

COMMENT ON TABLE payment IS
'Parent of two kinds of payments, each of which has an associated child table:

- Platform payments (payments requested between entities in the platform)
- Third party payments (payment requests across platform boundaries)';


--- PLATFORM PAYMENTS ---
CREATE TYPE platform_payment_status AS ENUM (
   'Unpaid',
   'Paid',
   'Deposited',
   'Cancelled',
   'Declined'
);

CREATE TABLE payment_platform(
   payment_id UUID PRIMARY KEY REFERENCES payment,
   status platform_payment_status NOT NULL,
   payer_org_id UUID NOT NULL REFERENCES organization(org_id),
   pay_by_account UUID REFERENCES account(account_id)
   --pay_by_card ...
   --pay_by_ach ...
);

SELECT add_created_at_column('payment_platform');
SELECT add_updated_at_column_and_trigger('payment_platform');

CREATE INDEX payment_platform_payer_org_id_idx ON payment_platform(payer_org_id);
CREATE INDEX payment_platform_pay_by_account_idx ON payment_platform (pay_by_account) WHERE pay_by_account IS NOT NULL;

COMMENT ON TABLE payment_platform IS
'Payments requested to entities within the platform.

An organization on the platform may request another organization on the platform to provide
a payment to them for services rendered.';

COMMENT ON COLUMN payment_platform.pay_by_account IS
'An organization who conducts business on the platform may choose
to pay by deducting funds from one of their accounts on the platform.

Alternatively, they may choose to pay by ACH or card
(NOTE: This option will be provided once Plaid/Stripe integration is implemented.)';

--- THIRD PARTY PAYMENTS ---
CREATE TYPE third_party_payment_status AS ENUM (
   'Unpaid',
   'Deposited',
   'Cancelled'
);

CREATE TABLE payment_third_party(
   payment_id UUID PRIMARY KEY REFERENCES payment,
   status_tp third_party_payment_status NOT NULL,
   payer_email VARCHAR(255) NOT NULL,
   payer_name VARCHAR(100) NOT NULL
   --pay_by_card ...
   --pay_by_ach ...
);

SELECT add_created_at_column('payment_third_party');
SELECT add_updated_at_column_and_trigger('payment_third_party');

COMMENT ON TABLE payment_third_party IS
'Payments requested to entities outside the platform.

An organization on the platform may request a payment from some organization
or contractor who does not conduct business on the platform.
They will receive an email requesting a payment and will be
be redirected to a secure form to pay by ACH or card.

TODO:
This table will be developed more after researching
Plaid and seeing what data will be necessary for paying by ACH/card.
This table is currently not in use.';


--- APPROVAL RULES ---
CREATE TYPE approvable_command AS ENUM (
   'InviteEmployee',
   'UpdateEmployeeRole',
   'UnlockCard',
   'ManageApprovalRule',
   'SendPayment',
   'SendInternalTransferBetweenOrgs',
   'SendDomesticTransfer'
);

CREATE TYPE command_approval_criteria AS ENUM (
   'AmountDailyLimit',
   'AmountPerCommand',
   'PerCommand'
);

CREATE TABLE command_approval_rule(
   rule_id UUID PRIMARY KEY,
   org_id UUID REFERENCES organization,
   command_type approvable_command NOT NULL,
   criteria command_approval_criteria NOT NULL,
   criteria_detail JSONB NOT NULL,
   permitted_approvers UUID[] NOT NULL
);

SELECT add_created_at_column('command_approval_rule');
SELECT add_updated_at_column_and_trigger('command_approval_rule');
SELECT add_soft_delete_column('command_approval_rule');

CREATE INDEX command_approval_rule_org_id_idx ON command_approval_rule(org_id) WHERE deleted_at IS NULL;

--- APPROVAL RULES to apply when a daily limit is exceeded ---
CREATE TABLE command_approval_rule_amount_daily_limit(
   rule_id UUID PRIMARY KEY REFERENCES command_approval_rule,
   org_id UUID REFERENCES organization,
   daily_limit MONEY NOT NULL
);

SELECT add_created_at_column('command_approval_rule_amount_daily_limit');
SELECT add_updated_at_column_and_trigger('command_approval_rule_amount_daily_limit');


--- APPROVAL RULES to apply when a account_event amount is within some range ---
CREATE TABLE command_approval_rule_amount_per_command(
   rule_id UUID PRIMARY KEY REFERENCES command_approval_rule,
   org_id UUID REFERENCES organization,
   lower_bound MONEY,
   upper_bound MONEY
);
-- write pre-check to verify lower bound or upper bound has been set

SELECT add_created_at_column('command_approval_rule_amount_per_command');
SELECT add_updated_at_column_and_trigger('command_approval_rule_amount_per_command');

--- COMMAND APPROVAL PROGRESS ---
CREATE TYPE command_approval_status AS ENUM ('Pending', 'Approved', 'Declined', 'Terminated');

CREATE TABLE command_approval_progress(
   command_id UUID PRIMARY KEY,
   rule_id UUID REFERENCES command_approval_rule,
   org_id UUID REFERENCES organization,
   requested_by_id UUID REFERENCES employee(employee_id),
   status command_approval_status NOT NULL,
   status_detail JSONB NOT NULL,
   approved_by UUID[],
   declined_by UUID REFERENCES employee(employee_id),
   command_type approvable_command NOT NULL,
   command_to_initiate_on_approval JSONB NOT NULL
);

SELECT add_created_at_column('command_approval_progress');
SELECT add_updated_at_column_and_trigger('command_approval_progress');


CREATE OR REPLACE PROCEDURE seed_balance_history()
AS $$
BEGIN
   INSERT INTO balance_history(account_id, date, balance)
   WITH RECURSIVE date_series AS (
      SELECT
         account_id,
         MIN(timestamp::date) AS start_date,
         (CURRENT_DATE - interval '1 day')::date AS end_date
      FROM account_event
      GROUP BY account_id

      UNION ALL

      SELECT
         account_id,
         (start_date + interval '1 day')::date,
         end_date
      FROM date_series
      WHERE start_date < end_date
   ),
   transactions_by_date AS (
      SELECT
         account_id,
         timestamp::date AS date,
         SUM(CASE WHEN money_flow = 'In' THEN amount::numeric ELSE -amount::numeric END) AS daily_diff
      FROM account_event
      GROUP BY account_id, timestamp::date
   )
   SELECT
      ds.account_id,
      ds.start_date AS date,

      COALESCE(
         SUM(tbd.daily_diff) OVER (
            PARTITION BY ds.account_id  -- calculate daily_diff sum for each account separately
            ORDER BY ds.start_date      -- calculate sum in chronological order
         ),
         0
      ) AS balance
   FROM date_series ds
   LEFT JOIN transactions_by_date tbd ON ds.account_id = tbd.account_id AND ds.start_date = tbd.date
   ORDER BY ds.account_id, ds.start_date
   ON CONFLICT(account_id, date) DO NOTHING;
END
$$ LANGUAGE plpgsql;

COMMENT ON PROCEDURE seed_balance_history() IS
'This procedure is used exclusively in the local/development database seeding
process to establish a balance history for a few months worth of seed data.
This is necessary for the analytics page time series chart.
See Account.App/AccountSeederActor.fs';


CREATE OR REPLACE PROCEDURE update_balance_history_for_yesterday()
AS $$
DECLARE
   yesterday CONSTANT DATE := CURRENT_DATE - '1 day'::interval;
BEGIN
   INSERT INTO balance_history (account_id, date, balance)
   SELECT
      account.account_id,
      yesterday as date,

      COALESCE(bh.balance, 0)
      +
      COALESCE(
         SUM(
            CASE
            WHEN ae.money_flow = 'In' THEN ae.amount::numeric
            ELSE -ae.amount::numeric
            END
         ),
         0
      ) AS balance
   FROM account
   LEFT JOIN account_event ae
      ON ae.account_id = account.account_id
      AND ae.money_flow IS NOT NULL
      AND ae.timestamp::date = yesterday
   JOIN balance_history bh
      ON bh.account_id = account.account_id
      -- Join on the balance_history date 1 day prior to yesterday.
      AND bh.date = yesterday - interval '1 day'
   GROUP BY account.account_id, bh.balance
   ON CONFLICT(account_id, date) DO NOTHING;
END
$$ LANGUAGE plpgsql;

COMMENT ON PROCEDURE update_balance_history_for_yesterday() IS
'This procedure is called daily by a cron job in order to compute the
balance history of all accounts for the previous day.
Shortly after midnight we need to compute the balance for the previous
day (CURRENT_DATE - 1).  We get the balance from balance history record
pertaining to (CURRENT_DATE - 2) summed up with the computed diff of
transaction amounts pertaining to (CURRENT_DATE - 1).';

CREATE OR REPLACE FUNCTION money_flow_time_series_daily(
   orgId UUID,
   startDate TIMESTAMPTZ,
   endDate TIMESTAMPTZ
)
RETURNS TABLE (
   day DATE,
   amount_in NUMERIC,
   amount_out NUMERIC,
   account_id UUID
) AS $$
BEGIN
   RETURN QUERY
   SELECT
      ds.day::date,

      COALESCE(
         SUM(ae.amount::numeric) filter(where ae.money_flow = 'In'),
         0
      ) AS amount_in,

      COALESCE(
         SUM(ae.amount::numeric) filter(where ae.money_flow = 'Out'),
         0
      ) AS amount_out,

      ids.account_id
   FROM generate_series(startDate, endDate, '1 day'::interval) AS ds(day)
   CROSS JOIN (
      SELECT account.account_id
      FROM account
      WHERE account.org_id = orgId
   ) ids
   LEFT JOIN account_event ae
      ON ae.account_id = ids.account_id
      AND ae.timestamp::date = ds.day::date
      AND ae.money_flow IS NOT NULL
      -- Exclude internal transfers within an org while
      -- still fetching internal transfers between orgs.
      AND ae.name NOT IN(
         'InternalTransferWithinOrgPending',
         'InternalTransferWithinOrgFailed',
         'InternalTransferWithinOrgDeposited',
         'InternalAutomatedTransferPending',
         'InternalAutomatedTransferFailed',
         'InternalAutomatedTransferDeposited'
      )
   GROUP BY ds.day, ids.account_id
   ORDER BY ds.day;
END
$$ LANGUAGE plpgsql;

CREATE TYPE monthly_time_series_filter_by AS ENUM ('Org', 'Account');

CREATE OR REPLACE FUNCTION money_flow_time_series_monthly(
   filterBy monthly_time_series_filter_by,
   filterId UUID, -- Either org_id or account_id depending on filterBy parameter
   lookbackMonths INT
)
RETURNS TABLE (
   month DATE,
   amount_in NUMERIC,
   amount_out NUMERIC
) AS $$
BEGIN
   RETURN QUERY
   SELECT
      months.month::date,

      COALESCE(
         SUM(ae.amount::numeric) filter(where ae.money_flow = 'In'),
         0
      ) AS amount_in,

      COALESCE(
         SUM(ae.amount::numeric) filter(where ae.money_flow = 'Out'),
         0
      ) AS amount_out
   FROM generate_series(
      DATE_TRUNC('month', CURRENT_DATE) - (lookbackMonths - 1 || ' months')::interval,
      CURRENT_DATE,
      '1 month'
   ) AS months(month)
   LEFT JOIN LATERAL (
      SELECT money_flow, amount
      FROM account_event
      WHERE
         CASE
            WHEN filterBy = 'Org' THEN org_id = filterId
            WHEN filterBy = 'Account' THEN account_id = filterId
         END
         AND money_flow IS NOT NULL
         AND DATE_TRUNC('month', timestamp) = months.month
         -- Exclude internal transfers within an org while
         -- still fetching internal transfers between orgs.
         AND name NOT IN(
            'InternalTransferWithinOrgPending',
            'InternalTransferWithinOrgFailed',
            'InternalTransferWithinOrgDeposited',
            'InternalAutomatedTransferPending',
            'InternalAutomatedTransferFailed',
            'InternalAutomatedTransferDeposited'
         )
   ) ae ON true
   GROUP BY months.month
   ORDER BY months.month;
END
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION money_flow_top_n_source_by_month(
   orgId UUID,
   flow money_flow,
   topN INT,
   d TIMESTAMPTZ
)
RETURNS TABLE (
   money_flow money_flow,
   amount NUMERIC,
   source VARCHAR
) AS $$
BEGIN
   RETURN QUERY
   SELECT
      flow AS money_flow,
      COALESCE(SUM(ae.amount::numeric), 0) AS amount,
      ae.source
   FROM account_event ae
   WHERE
      org_id = orgId
      AND ae.money_flow = flow
      AND timestamp::date
         BETWEEN DATE_TRUNC('month', d)
         AND (DATE_TRUNC('month', d) + INTERVAL '1 month' - INTERVAL '1 day')
      -- Exclude internal transfers within an org while
      -- still fetching internal transfers between orgs.
      AND name NOT IN(
         'InternalTransferWithinOrgPending',
         'InternalTransferWithinOrgFailed', 
         'InternalTransferWithinOrgDeposited',
         'InternalAutomatedTransferPending',
         'InternalAutomatedTransferFailed',
         'InternalAutomatedTransferDeposited'
      )
   GROUP BY ae.source
   ORDER BY amount DESC
   LIMIT topN;
END
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION purchase_top_n_employees_by_month(
   orgId UUID,
   topN INT,
   d TIMESTAMPTZ
)
RETURNS TABLE (
   employee_id UUID,
   employee_name TEXT,
   amount NUMERIC
) AS $$
BEGIN
   RETURN QUERY
   SELECT
      employee_event.employee_id,
      employee.first_name || ' ' || employee.last_name as employee_name,
      COALESCE(SUM(ae.amount::numeric), 0) AS amount
   FROM employee_event
   JOIN account_event ae using(correlation_id)
   JOIN employee using(employee_id)
   WHERE
      employee_event.org_id = orgId
      AND employee_event.name = 'PurchaseConfirmedByAccount'
      AND employee_event.timestamp::date
         BETWEEN DATE_TRUNC('month', d)
         AND (DATE_TRUNC('month', d) + INTERVAL '1 month' - INTERVAL '1 day')
   GROUP BY employee_event.employee_id, employee_name
   ORDER BY amount DESC
   LIMIT topN;
END
$$ LANGUAGE plpgsql;

CREATE VIEW daily_purchase_accrued AS
SELECT
   account_id,
   COALESCE(SUM(amount::numeric), 0) as amount_accrued
FROM account_event
WHERE
   amount IS NOT NULL
   AND name = 'DebitedAccount'
   AND timestamp::date = CURRENT_DATE
GROUP BY account_id;

CREATE VIEW monthly_purchase_accrued AS
SELECT
   account_id,
   COALESCE(SUM(amount::numeric), 0) as amount_accrued
FROM account_event
WHERE
   amount IS NOT NULL
   AND name = 'DebitedAccount'
   AND timestamp::date >= date_trunc('month', CURRENT_DATE)
GROUP BY account_id;

CREATE VIEW daily_purchase_accrued_by_card AS
SELECT
   card_id,
   COALESCE(SUM(amount::numeric), 0) as amount_accrued
FROM account_event
WHERE
   amount IS NOT NULL
   AND name = 'DebitedAccount'
   AND timestamp::date = CURRENT_DATE
GROUP BY card_id;

CREATE VIEW monthly_purchase_accrued_by_card AS
SELECT
   card_id,
   COALESCE(SUM(amount::numeric), 0) as amount_accrued
FROM account_event
WHERE
   amount IS NOT NULL
   AND name = 'DebitedAccount'
   AND timestamp::date >= date_trunc('month', CURRENT_DATE)
GROUP BY card_id;

CREATE TYPE time_frame AS ENUM ('Day', 'Month');

CREATE OR REPLACE FUNCTION transfer_accrued(
   orgId UUID,
   timeFrame time_frame
)
RETURNS TABLE (
   account_id UUID,
   internal_transfer_within_org_accrued NUMERIC,
   internal_transfer_between_orgs_accrued NUMERIC,
   domestic_transfer_accrued NUMERIC,
   payment_paid_accrued NUMERIC
) AS $$
BEGIN
  RETURN QUERY
  SELECT
     account.account_id,

     COALESCE(
        SUM(
           CASE
           WHEN ae.name IN(
              'InternalTransferWithinOrgPending',
              'InternalAutomatedTransferPending'
           )
           THEN ae.amount::numeric

           WHEN ae.name IN(
              'InternalTransferWithinOrgFailed',
              'InternalAutomatedTransferFailed'
           )
           THEN -ae.amount::numeric

           ELSE 0
           END
        ),
        0
     ) AS internal_transfer_within_org_accrued,

     COALESCE(
        SUM(
           CASE
           WHEN ae.name = 'InternalTransferBetweenOrgsPending'
           THEN ae.amount::numeric

           WHEN ae.name = 'InternalTransferBetweenOrgsFailed'
           THEN -ae.amount::numeric

           ELSE 0
           END
        ),
        0
     ) AS internal_transfer_between_orgs_accrued,

     COALESCE(
        SUM(
           CASE
           WHEN ae.name = 'DomesticTransferPending' THEN ae.amount::numeric
           WHEN ae.name = 'DomesticTransferFailed' THEN -ae.amount::numeric
           ELSE 0
           END
        ),
        0
     ) AS domestic_transfer_accrued,

     COALESCE(
        SUM(
           CASE
           WHEN ae.name = 'PlatformPaymentPaid' THEN ae.amount::numeric
           ELSE 0
           END
        ),
        0
     ) AS payment_paid_accrued
  FROM account_event ae
  JOIN account using(account_id)
  -- Account events related to transfers are represented as "transfer" read models.
  -- Account events related to platform payments are represented as "payment_platform" read models.
  -- Use LEFT JOIN instead of JOIN on transfer or we will miss out on payments paid metrics.
  LEFT JOIN transfer ON ae.correlation_id = transfer.transfer_id
  WHERE
     ae.org_id = orgId
     AND ae.amount IS NOT NULL
     AND ae.name IN (
        'InternalAutomatedTransferPending', 'InternalAutomatedTransferFailed',
        'InternalTransferWithinOrgPending', 'InternalTransferWithinOrgFailed',
        'InternalTransferBetweenOrgsPending', 'InternalTransferBetweenOrgsFailed',
        'DomesticTransferPending', 'DomesticTransferFailed',
        'PlatformPaymentPaid'
     )
     AND (account.last_billing_cycle_at IS NULL OR ae.timestamp > account.last_billing_cycle_at)
     AND
       CASE
       WHEN timeFrame = 'Day'
       THEN
          CASE
          WHEN transfer.scheduled_at IS NOT NULL
          THEN transfer.scheduled_at::date = CURRENT_DATE

          WHEN ae.name = 'PlatformPaymentPaid'
          THEN ae.timestamp::date = CURRENT_DATE

          ELSE false
          END
       WHEN timeFrame = 'Month'
       THEN
          CASE
          WHEN ae.name = 'PlatformPaymentPaid'
          THEN ae.timestamp::date >= date_trunc('month', CURRENT_DATE)
          -- Account event corresponds to a transfer rather than a payment.
          -- Need to check it's scheduled_at date rather than account_event.timestamp.
          ELSE transfer.scheduled_at::date >= date_trunc('month', CURRENT_DATE)
          END
       END
  GROUP BY account.account_id;
END
$$ LANGUAGE plpgsql;

/**
 * SEED DATA:
 * NOTE:
 * This seed data is necessary for the app to work in any environment
 * (local, production, staging, etc.).
 *
 * Seed data specific to local dev environment & demonstration purposes
 * is created via actor messages in Account.App/AccountSeederActor.fs
**/

/**
 * Create a "system" user to represent account events which do not originate
 * from a human user.  Used in BillingCycleCommand, MaintenanceFeeCommand, etc.
**/
INSERT INTO organization (org_name, status, status_detail)
VALUES ('system', 'Active', '"Active"'::jsonb);

INSERT INTO employee (
   employee_id,
   email,
   first_name,
   last_name,
   role,
   status,
   status_detail,
   pending_purchases,
   onboarding_tasks,
   cards,
   org_id
)
VALUES (
    -- This employee_id is defined in Lib.SharedClientServer/Constants.fs as
    -- SYSTEM_USER_ID.  Account commands such as BillingCycle, which do not originate
    -- from a human, are created with initiated_by_id set to SYSTEM_USER_ID.
   '029528ee-a120-4301-b8b5-e9c60d859346',
   'system@gmail.com',
   'system',
   'system',
   'Admin',
   'Active',
   '"Active"'::jsonb,
   '{}'::jsonb,
   '{}'::jsonb,
   '{}'::jsonb,
   (SELECT org_id FROM organization WHERE org_name = 'system')
);

INSERT INTO category (name)
VALUES
   ('Airlines'),
   ('Alcohol and Bars'),
   ('Books'),
   ('Car Rental'),
   ('Charity'),
   ('Clothing'),
   ('Conferences'),
   ('Education'),
   ('Electronics'),
   ('Entertainment'),
   ('Food Delivery'),
   ('Grocery'),
   ('Internet and Telephone'),
   ('Legal'),
   ('Lodging'),
   ('Medical'),
   ('Memberships'),
   ('Office Supplies'),
   ('Parking'),
   ('Restaurants'),
   ('Retail'),
   ('Rideshare and Taxis'),
   ('Shipping'),
   ('Software'),
   ('Utilities'),
   ('Vehicle Expenses'),
   ('Other');

commit;
