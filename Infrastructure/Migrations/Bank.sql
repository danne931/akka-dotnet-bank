SET timezone = 'America/Los_Angeles';

DROP TABLE IF EXISTS billingstatements;
DROP TABLE IF EXISTS users;
DROP TABLE IF EXISTS accounts;

CREATE TABLE accounts (
    id UUID PRIMARY KEY,
    email VARCHAR(255) UNIQUE,
    first_name VARCHAR(50) NOT NULL,
    last_name VARCHAR(50) NOT NULL,
    balance MONEY NOT NULL,
    currency VARCHAR(3) NOT NULL,
    status VARCHAR(50) NOT NULL,
    daily_debit_limit MONEY,
    daily_debit_accrued MONEY NOT NULL,
    last_debit_date TIMESTAMPTZ,
    transfer_recipients JSONB NOT NULL,
    maintenance_fee_qualifying_deposit_found BOOLEAN NOT NULL,
    maintenance_fee_daily_balance_threshold BOOLEAN NOT NULL,
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE users (
    first_name VARCHAR(50) NOT NULL,
    last_name VARCHAR(50) NOT NULL,
    email VARCHAR(255) UNIQUE,
    account_id UUID UNIQUE REFERENCES accounts (id) ON DELETE CASCADE,
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    id UUID PRIMARY KEY DEFAULT gen_random_uuid()
);

CREATE TABLE billingstatements (
    name VARCHAR(100) NOT NULL,
    account_id UUID NOT NULL REFERENCES accounts (id) ON DELETE CASCADE,
    transactions JSONB NOT NULL,
    balance MONEY NOT NULL,
    month INT NOT NULL,
    year INT NOT NULL,
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    id UUID PRIMARY KEY DEFAULT gen_random_uuid()
);
