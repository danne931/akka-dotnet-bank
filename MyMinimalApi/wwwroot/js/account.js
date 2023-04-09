'use strict'

let transferRecipients = {}

const connection = new signalR.HubConnectionBuilder()
  .withUrl('/accountHub')
  .build()

connection.on('ReceiveError', function (err) {
  const msg = `Error: ${err}`
  notifyError(msg)
})

connection.on('ReceiveMessage', function (stateTransition) {
  renderAccountState(stateTransition.newState)
  renderEventIntoListView(stateTransition.event)
})

const accountId = 'ec3e94cc-eba1-4ff4-b3dc-55010ecf67e9'
connection
  .start()
  .then(() => connection.invoke('AddToConnectionGroup', accountId))
  .then(() => Promise.all([
    fetch(`/accounts/${accountId}`),
    fetch(`/diagnostic/events/${accountId}`)
  ]))
  .then(responses =>
    Promise.all(responses.map(res => {
      if (!res.ok) {
        throw new Error(`Http status: ${res.status}`)
      }
      return res.json()
    }))
  )
  .then(([account, events]) => {
    renderAccountState(account)
    interpolateTransferRecipientSelection(account)
    transferRecipients = account.transferRecipients
    events.forEach(renderEventIntoListView);
  })
  .catch(notifyError)

function renderAccountState (account) {
  document.getElementById('account-balance').textContent =
    `$${account.balance}`
  document.getElementById('account-name').textContent =
    `${account.firstName} ${account.lastName}`
  document
    .getElementById('account-daily-debit-limit')
    .textContent = account.dailyDebitLimit !== -1
      ? account.dailyDebitLimit
      : 'No Limit'
  document
    .getElementById('account-daily-debit-accrued')
    .textContent = account.dailyDebitAccrued
  document.getElementById('account-debit-card-lock').checked =
    // TODO: Send string to client instead of enum
    account.status === 1
}

function interpolateTransferRecipientSelection (account) {
  const transferRecipientsAsNodes = Object.entries(account.transferRecipients)
    .map(([key, val]) => {
      const option = document.createElement('option')
      option.value = key
      option.label = `${val.firstName} ${val.lastName}`
      return option
    })
  document
    .querySelector('#account-transfer-form select')
    .replaceChildren(...transferRecipientsAsNodes)
}

function renderEventIntoListView (evt) {
  const li = document.createElement('li')
  li.textContent = eventToTransactionString(evt)
  document.getElementById('messagesList').prepend(li)
}

function eventToTransactionString (evt) {
  switch (evt.name) {
    case 'CreatedAccount':
      return `Created account for ${evt.firstName} ${evt.lastName} on ${evt.timestamp}`
    case 'DepositedCash':
      return `$${evt.depositedAmount} deposited on ${evt.timestamp}`
    case 'DebitedAccount':
      return `$${evt.debitedAmount} debited from account by ${evt.origin} on ${evt.timestamp}`
    case 'DailyDebitLimitUpdated':
      return `Daily debit limit updated to ${evt.debitLimit} on ${evt.timestamp}`
    case 'LockedCard':
      return `Debit card locked on ${evt.timestamp}`
    case 'UnlockedCard':
      return `Debit card unlocked on ${evt.timestamp}`
    case 'DebitedTransfer':
      return `$${evt.debitedAmount} transferred to ${evt.recipient.firstName} ${evt.recipient.lastName} on ${evt.timestamp}`
    case 'RegisteredInternalTransferRecipient':
      return `Registered transfer recipient ${evt.firstName} ${evt.lastName} on ${evt.timestamp}`
    default:
      return 'Unknown event'
  }
}

function cardLockToggled () {
  const url = document.getElementById('account-debit-card-lock').checked
    ? '/accounts/lock'
    : '/accounts/unlock'
  jsonPost(url, { entityId: accountId })
}

listenForFormSubmit(
  document.getElementById('account-deposit-form'),
  '/accounts/deposit',
  formData => ({
    entityId: accountId,
    amount: formData.get('account-deposit-amount')
  })
)

listenForFormSubmit(
  document.getElementById('account-debit-form'),
  '/accounts/debit',
  formData => ({
    entityId: accountId,
    amount: formData.get('account-debit-amount'),
    origin: formData.get('account-debit-origin')
  })
)

listenForFormSubmit(
  document.getElementById('account-daily-debit-limit-form'),
  '/accounts/daily-debit-limit',
  formData => ({
    entityId: accountId,
    debitLimit: formData.get('account-daily-debit-limit')
  })
)

listenForFormSubmit(
  document.getElementById('account-transfer-form'),
  '/transfers',
  formData => ({
    entityId: accountId,
    amount: formData.get('account-transfer-amount'),
    recipient: getTransferRecipient(
      formData.get('account-transfer-recipient-id')
    )
  })
)

function listenForFormSubmit (formEl, url, formDataToProps) {
  formEl.addEventListener('submit', e => {
    e.preventDefault()
    jsonPost(url, formDataToProps(new FormData(formEl)))
      .then(_ => formEl.reset())
  })
}

function getTransferRecipient (recipientId) {
  return transferRecipients[recipientId]
}

function jsonPost (url, props) {
  return fetch(url, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(props)
  })
  .catch(notifyError)
}

function notifyError (err) {
  const errMsg = err.toString()
  console.error(errMsg)
  alert(errMsg)
}