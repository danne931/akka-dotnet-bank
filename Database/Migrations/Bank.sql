begin;

CREATE EXTENSION IF NOT EXISTS pg_trgm;

DROP VIEW IF EXISTS daily_purchase_accrued;
DROP VIEW IF EXISTS monthly_purchase_accrued;
DROP VIEW IF EXISTS daily_transfer_accrued;

DROP TABLE IF EXISTS billingstatement;
DROP TABLE IF EXISTS merchant;
DROP TABLE IF EXISTS ancillarytransactioninfo;
DROP TABLE IF EXISTS transaction;
DROP TABLE IF EXISTS card;
DROP TABLE IF EXISTS account;
DROP TABLE IF EXISTS employee_event;
DROP TABLE IF EXISTS employee;
DROP TABLE IF EXISTS organization;
DROP TABLE IF EXISTS category;

DROP TYPE IF EXISTS money_flow;
DROP TYPE IF EXISTS employee_status;
DROP TYPE IF EXISTS employee_role;
DROP TYPE IF EXISTS account_depository;
DROP TYPE IF EXISTS account_status;
DROP TYPE IF EXISTS card_status;
DROP TYPE IF EXISTS card_type;

CREATE TABLE organization (
   org_id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
   org_name VARCHAR(100) UNIQUE NOT NULL,
   requires_employee_invite_approval BOOLEAN NOT NULL,
   created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

CREATE TYPE account_depository AS ENUM ('checking', 'savings');
CREATE TYPE account_status AS ENUM ('pending', 'active', 'closed', 'readyfordelete');
CREATE TABLE account (
   account_id UUID PRIMARY KEY,
   routing_number INT NOT NULL,
   account_number BIGINT UNIQUE NOT NULL,
   account_name VARCHAR(50) NOT NULL,
   depository account_depository NOT NULL,
   balance MONEY NOT NULL,
   currency VARCHAR(3) NOT NULL,
   status account_status NOT NULL,
   internal_transfer_recipients JSONB NOT NULL,
   domestic_transfer_recipients JSONB NOT NULL,
   internal_transfer_senders JSONB NOT NULL,
   maintenance_fee_qualifying_deposit_found BOOLEAN NOT NULL,
   maintenance_fee_daily_balance_threshold BOOLEAN NOT NULL,
   events JSONB NOT NULL,
   in_progress_internal_transfers JSONB NOT NULL,
   in_progress_internal_transfers_count INT NOT NULL,
   in_progress_domestic_transfers JSONB NOT NULL,
   in_progress_domestic_transfers_count INT NOT NULL,
   failed_domestic_transfers JSONB NOT NULL,
   failed_domestic_transfers_count INT NOT NULL,
   org_id UUID NOT NULL REFERENCES organization,
   created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
   last_billing_cycle_at TIMESTAMPTZ
);

CREATE INDEX account_in_progress_domestic_transfers_count_idx ON account(in_progress_domestic_transfers_count);
CREATE INDEX account_last_billing_cycle_at_idx ON account(last_billing_cycle_at);

CREATE TABLE billingstatement (
   name VARCHAR(100) NOT NULL,
   account_id UUID NOT NULL REFERENCES account ON DELETE CASCADE,
   last_persisted_event_sequence_number BIGINT NOT NULL,
   transactions JSONB NOT NULL,
   balance MONEY NOT NULL,
   month INT NOT NULL,
   year INT NOT NULL,
   account_snapshot BYTEA NOT NULL,
   created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
   id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
   org_id UUID NOT NULL REFERENCES organization
);

CREATE TABLE category (
   category_id SMALLSERIAL PRIMARY KEY,
   name VARCHAR(100) UNIQUE NOT NULL,
   created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE merchant (
   org_id UUID NOT NULL REFERENCES organization,
   name VARCHAR(100) NOT NULL,
   alias VARCHAR(100),
   created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,

   PRIMARY KEY (org_id, name)
);

INSERT INTO category (name)
VALUES
   ('Advertising'),
   ('Airlines'),
   ('Alcohol and Bars'),
   ('Books and Newspaper'),
   ('Car Rental'),
   ('Charity'),
   ('Clothing'),
   ('Conferences'),
   ('Education'),
   ('Electronics'),
   ('Entertainment'),
   ('Facilities Expenses'),
   ('Fees'),
   ('Food Delivery'),
   ('Fuel and Gas'),
   ('Gambling'),
   ('Government Services'),
   ('Grocery'),
   ('Ground Transportation'),
   ('Insurance'),
   ('Internet and Telephone'),
   ('Legal'),
   ('Lodging'),
   ('Medical'),
   ('Memberships'),
   ('Office Supplies'),
   ('Parking'),
   ('Political'),
   ('Professional Services'),
   ('Restaurants'),
   ('Retail'),
   ('Rideshare and Taxis'),
   ('Shipping'),
   ('Software'),
   ('Taxes'),
   ('Travel'),
   ('Utilities'),
   ('Vehicle Expenses'),
   ('Other');

CREATE TYPE employee_status AS ENUM (
  'pendinginviteconfirmation',
  'pendinginviteapproval',
  'pendingrestoreaccessapproval',
  'active',
  'closed',
  'readyfordelete'
);
CREATE TYPE employee_role AS ENUM ('admin', 'scholar', 'cardonly');
CREATE TABLE employee (
   employee_id UUID PRIMARY KEY,
   email VARCHAR(255) UNIQUE NOT NULL,
   first_name VARCHAR(50) NOT NULL,
   last_name VARCHAR(50) NOT NULL,
   search_query TEXT,
   role employee_role NOT NULL,
   status employee_status NOT NULL,
   pending_purchases JSONB NOT NULL,
   onboarding_tasks JSONB NOT NULL,
   cards JSONB NOT NULL,
   invite_token UUID,
   invite_expiration TIMESTAMPTZ,
   auth_provider_user_id UUID,
   org_id UUID NOT NULL REFERENCES organization,
   created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);
CREATE INDEX employee_email_idx ON employee(email);
CREATE INDEX employee_invite_token_idx ON employee(invite_token);
CREATE INDEX employee_search_query_idx ON employee USING gist (search_query gist_trgm_ops);

CREATE OR REPLACE FUNCTION update_search_query() RETURNS TRIGGER AS $$
BEGIN
  NEW.search_query = concat_ws(' ', NEW.first_name, NEW.last_name, NEW.email);
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER update_search_query_trigger
BEFORE INSERT OR UPDATE ON employee
FOR EACH ROW
EXECUTE FUNCTION update_search_query();

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

CREATE TYPE card_status AS ENUM ('active', 'frozen', 'closed');
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
   org_id UUID NOT NULL REFERENCES organization,
   created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

CREATE TYPE money_flow AS ENUM ('in', 'out');
CREATE TABLE transaction (
   name VARCHAR(50) NOT NULL,
   amount MONEY,
   money_flow money_flow,
   timestamp TIMESTAMPTZ NOT NULL,
   transaction_id UUID PRIMARY KEY,
   account_id UUID NOT NULL REFERENCES account ON DELETE CASCADE,
   correlation_id UUID NOT NULL,
   card_id UUID REFERENCES card,
   event JSONB NOT NULL,
   org_id UUID NOT NULL REFERENCES organization
);
CREATE INDEX transaction_accrued_amount_view_query_idx
ON transaction(amount, name, timestamp);

CREATE TABLE ancillarytransactioninfo (
   note TEXT,
   category_id SMALLSERIAL REFERENCES category,
   transaction_id UUID PRIMARY KEY REFERENCES transaction ON DELETE CASCADE
);
ALTER TABLE ancillarytransactioninfo
ALTER COLUMN category_id DROP NOT NULL;

CREATE VIEW daily_purchase_accrued AS
SELECT
   card_id,
   COALESCE(SUM(amount::NUMERIC), 0) as amount_accrued
FROM transaction
WHERE
   amount IS NOT NULL
   AND name = 'DebitedAccount'
   AND timestamp::DATE = CURRENT_DATE
GROUP BY card_id;

CREATE VIEW monthly_purchase_accrued AS
SELECT
   card_id,
   COALESCE(SUM(amount::NUMERIC), 0) as amount_accrued
FROM transaction
WHERE
   amount IS NOT NULL
   AND name = 'DebitedAccount'
   AND timestamp::DATE >= date_trunc('month', CURRENT_DATE)
GROUP BY card_id;

CREATE VIEW daily_transfer_accrued AS
SELECT
   account.account_id,

   COALESCE(
      SUM(
         CASE 
         WHEN t.name = 'InternalTransferPending' THEN t.amount::NUMERIC
         WHEN t.name = 'InternalTransferRejected' THEN -t.amount::NUMERIC
         ELSE 0
         END
      ),
      0
   ) AS internal_transfer_accrued,

   COALESCE(
      SUM(
         CASE 
         WHEN t.name = 'DomesticTransferPending' THEN t.amount::NUMERIC
         WHEN t.name = 'DomesticTransferRejected' THEN -t.amount::NUMERIC
         ELSE 0
         END
      ),
      0
   ) AS domestic_transfer_accrued
FROM transaction t
LEFT JOIN account using(account_id)
WHERE
   t.amount IS NOT NULL
   AND t.name IN (
      'InternalTransferPending', 'DomesticTransferPending',
      'InternalTransferRejected', 'DomesticTransferRejected'
   )
   AND (account.last_billing_cycle_at IS NULL OR t.timestamp > account.last_billing_cycle_at)
   -- TODO: Create internal/domestic transfer read model tables 
   --       and read from scheduled_date column.
   AND (t.event #>> '{1,Data,BaseInfo,ScheduledDate}')::timestamptz::date = CURRENT_DATE
GROUP BY account.account_id;

commit;
