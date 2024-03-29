'use strict'

const state = {
  accounts: {},
  selectedAccountId: null,
  transferRecipients: {},
  redirectTransferToCreateRecipientView: false,
  validationErrorModalOpen: false
}

const selectors = {
  accountsList: () => document.getElementById('accounts-list'),
  accountBalance: () => document.getElementById('account-balance'),
  accountName: () => document.getElementById('account-name'),
  dailyDebitLimit: () => document.getElementById('account-daily-debit-limit'),
  dailyDebitAccrued: () => document.getElementById('account-daily-debit-accrued'),
  debitCardLock: () => document.getElementById('account-debit-card-lock'),
  transferRecipientSelection: () => document.querySelector('#account-transfer-form select'),
  eventList: () => document.getElementById('event-list'),
  progressIndicator: () => document.querySelector('progress[data-transactions-loader]'),
  actionWrapper: () => document.getElementById('action-wrapper'),
  form: {
    debit: () => document.getElementById('account-debit-form'),
    deposit: () => document.getElementById('account-deposit-form'),
    transfer: () => document.getElementById('account-transfer-form'),
    transferRecipient: () => document.getElementById('register-transfer-recipient-form'),
    dailyDebitLimit: () => document.getElementById('account-daily-debit-limit-form')
  },
  validationErrorModal: () => document.getElementById('validation-error-modal'),
  validationErrorReason: () => document.getElementById('validation-error-reason')
}

const eventsToIgnore = {
  MaintenanceFeeSkipped: true
}

const connection = new signalR.HubConnectionBuilder()
  .withUrl('/accountHub')
  .build()

connection.on('ReceiveError', function (err) {
  console.error(`Error: ${err}`)
  openValidationErrorModal(err)
})

connection.on('ReceiveMessage', function ({ newState, event }) {
  if (eventsToIgnore[event.name]) return

  renderAccountState(newState)
  renderEventIntoListView(event)

  if (event.name === 'RegisteredInternalTransferRecipient') {
    interpolateTransferRecipientSelection(newState)
    state.transferRecipients = newState.transferRecipients
  }
})

connection
  .start()
  .then(() => fetch('/accounts'))
  .then(res => {
    if (!res.ok) {
      throw new Error(`Http status: ${res.status}`)
    }
    return res.json()
  })
  .then(accounts => {
    state.accounts = accounts
    renderAccountsList(accounts)
    return accountSelected(accounts[0].entityId)
  })
  .catch(notifyError)

function accountSelectionClicked (e) {
  e.preventDefault()
  // Close account list selection
  selectors
    .accountsList()
    .closest('details')
    .removeAttribute('open')

  const accountId = e.target.getAttribute('value')
  if (accountId === state.selectedAccountId) return

  renderTransactionsLoaderStart()

  accountSelected(accountId)
    .then(() => renderTransactionsLoaderFinish())
}

function accountSelected (accountId) {
  return addAccountToConnectionGroup(accountId)
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
      state.transferRecipients = account.transferRecipients
      renderEventsIntoListView(events.reverse())
      renderTransactionsLoaderFinish()
    })
}

function addAccountToConnectionGroup (accountId) {
  const promise = state.selectedAccountId != null
    ? connection.invoke('RemoveFromConnectionGroup', state.selectedAccountId)
    : Promise.resolve()

  return promise
    .then(() => connection.invoke('AddToConnectionGroup', accountId))
    .then(() => {
      state.selectedAccountId = accountId
      highlightSelectedAccount(accountId)
    })
}

function renderAccountsList (accounts) {
  const accountsAsNodes = accounts.reduce((acc, account) => {
    const li = document.createElement('li')
    const a = document.createElement('a')
    a.setAttribute('href', '')
    a.setAttribute('value', account.entityId)
    a.setAttribute('onclick', 'accountSelectionClicked(event)')
    a.textContent = `${account.firstName} ${account.lastName} - Account Id: ${account.entityId}`
    li.append(a)
    acc.push(li)
    return acc
  }, [])

  selectors.accountsList().prepend(...accountsAsNodes)
}

function highlightSelectedAccount (accountId) {
  document.querySelectorAll('#accounts-list a').forEach(node => {
    node.getAttribute('value') === accountId
       ? node.classList.add('selected')
       : node.classList.remove('selected')
  })
}

function renderAccountState (account) {
  selectors.accountBalance().textContent = `$${account.balance}`
  selectors.accountName().textContent = `${account.firstName} ${account.lastName}`
  selectors.dailyDebitLimit().textContent = account.dailyDebitLimit !== -1
    ? `$${account.dailyDebitLimit}`
    : '-'
  selectors.dailyDebitAccrued().textContent = `$${account.dailyDebitAccrued}`
  selectors.debitCardLock().checked = account.status === 1
}

function interpolateTransferRecipientSelection (account) {
  const transferRecipientsAsNodes = Object.entries(account.transferRecipients)
    .map(([key, val]) => {
      const option = document.createElement('option')
      option.value = key
      option.label = `${val.firstName} ${val.lastName}`
      return option
    })

  selectors
    .transferRecipientSelection()
    .replaceChildren(...transferRecipientsAsNodes)
}

function renderEventsIntoListView (events) {
  const rows = events.reduce((acc, val) => {
    if (!eventsToIgnore[val.name]) acc.push(eventToTableRow(val))
    return acc
  }, [])

  selectors
    .eventList()
    .replaceChildren(...rows)
}

function renderEventIntoListView (evt) {
  selectors.eventList().prepend(eventToTableRow(evt))
}

function renderTransactionsLoaderStart () {
  selectors.progressIndicator().removeAttribute('value')
}

function renderTransactionsLoaderFinish () {
  selectors.progressIndicator().setAttribute('value', 100)
}

function eventToTableRow (evt) {
  const tr = document.createElement('tr')
  const th = document.createElement('th')
  th.setAttribute('scope', 'row')
  const amountEl = document.createElement('td')
  const eventEl = document.createElement('td')
  const originEl = document.createElement('td')
  const dateEl = document.createElement('td')
  const infoEl = document.createElement('td')

  const rowProps = {
    date: evt.timestamp,
    name: 'Unknown Event',
    origin: '-',
    amount: '-',
    info: ''
  }

  switch (evt.name) {
    case 'CreatedAccount':
      rowProps.name = 'Account Created'
      rowProps.amount = `$${evt.balance}`
      amountEl.classList.add('credit')
      break
    case 'DepositedCash':
      rowProps.name = 'Deposit'
      rowProps.amount = `$${evt.depositedAmount}`
      rowProps.origin = evt.origin
      amountEl.classList.add('credit')
      break
    case 'DebitedAccount':
      rowProps.name = 'Debit'
      rowProps.amount = `-$${evt.debitedAmount}`
      rowProps.origin = evt.origin
      amountEl.classList.add('debit')
      break
    case 'MaintenanceFeeDebited':
      rowProps.name = 'Maintenance Fee'
      rowProps.amount = `-$${evt.debitedAmount}`
      amountEl.classList.add('debit')
      break
    case 'DailyDebitLimitUpdated':
      rowProps.name = 'Daily Debit Limit Updated'
      rowProps.amount = `$${evt.debitLimit}`
      break
    case 'LockedCard':
      rowProps.name = 'Card Locked'
      break
    case 'UnlockedCard':
      rowProps.name = 'Card Unlocked'
      break
    case 'TransferPending':
      rowProps.name = 'Pending Transfer'
      rowProps.info = `Recipient: ${evt.recipient.firstName} ${evt.recipient.lastName}`
      rowProps.amount = `-$${evt.debitedAmount}`
      amountEl.classList.add('debit')
      break
    case 'TransferApproved':
      rowProps.name = 'Approved Transfer'
      rowProps.info = `Recipient: ${evt.recipient.firstName} ${evt.recipient.lastName}`
      rowProps.amount = `$${evt.debitedAmount}`
      break
    case 'TransferRejected':
      rowProps.amount = `+$${evt.debitedAmount}`
      rowProps.name = 'Rejected Transfer'
      rowProps.info = `Recipient: ${evt.recipient.firstName} ${evt.recipient.lastName} - Reason: ${evt.reason} -  Account refunded`
      amountEl.classList.add('credit')
      break
    case 'RegisteredInternalTransferRecipient':
      rowProps.name = `Registered Internal Transfer Recipient`
      rowProps.info = `Recipient: ${evt.firstName} ${evt.lastName}`
      break
  }

  if (rowProps.name === 'Unknown Event') {
    console.error('unknown event..')
    console.log(evt)
  }
  amountEl.append(rowProps.amount)
  eventEl.append(rowProps.name)
  originEl.append(rowProps.origin)
  dateEl.append(rowProps.date)
  infoEl.append(rowProps.info)

  tr.replaceChildren(...[th, amountEl, eventEl, originEl, dateEl, infoEl])
  return tr
}

function accountActionButtonClicked (action) {
  selectors.actionWrapper().classList.add('hidden')

  // If user wants to view the transfer form but hasn't already added
  // recipients, redirect them to the transfer recipients creation form
  // first.  Once they submit a recipient then transition the view to their
  // intended transfer form.
  if (action === 'transfer' && !Object.keys(state.transferRecipients).length) {
    action = 'transferRecipient'
    state.redirectTransferToCreateRecipientView = true
  }

  renderAccountActionForm(action)
}

function renderAccountActionForm (action) {
  selectors
    .form[action]()
    .closest('.form-wrapper')
    .classList
    .remove('hidden')
}

function accountActionBackButtonClicked (e) {
  closeFormView(e.target.closest('.form-wrapper').querySelector('form'))
  renderAccountActionList()
}

function closeFormView (formEl) {
  formEl.reset()
  formEl.closest('.form-wrapper').classList.add('hidden')
}

function renderAccountActionList () {
  selectors.actionWrapper().classList.remove('hidden')
}

function resetDomAfterRecipientRegistration () {
  if (state.redirectTransferToCreateRecipientView) {
    state.redirectTransferToCreateRecipientView = false
    renderAccountActionForm('transfer')
  } else {
    renderAccountActionList()
  }
}

function cardLockToggled () {
  const url = selectors.debitCardLock().checked ? '/accounts/lock' : '/accounts/unlock'
  jsonPost(url, { entityId: state.selectedAccountId })
}

listenForFormSubmit(
  selectors.form.deposit(),
  '/accounts/deposit',
  formData => ({
    entityId: state.selectedAccountId,
    amount: formData.get('account-deposit-amount')
  })
)

listenForFormSubmit(
  selectors.form.debit(),
  '/accounts/debit',
  formData => ({
    entityId: state.selectedAccountId,
    amount: formData.get('account-debit-amount'),
    origin: formData.get('account-debit-origin')
  })
)

listenForFormSubmit(
  selectors.form.dailyDebitLimit(),
  '/accounts/daily-debit-limit',
  formData => ({
    entityId: state.selectedAccountId,
    debitLimit: formData.get('account-daily-debit-limit')
  })
)

listenForFormSubmit(
  selectors.form.transfer(),
  '/transfers',
  formData => ({
    entityId: state.selectedAccountId,
    amount: formData.get('account-transfer-amount'),
    date: new Date(),
    recipient: getTransferRecipient(
      formData.get('account-transfer-recipient-id')
    )
  })
)

listenForFormSubmit(
  selectors.form.transferRecipient(),
  '/transfers/register-recipient',
  formData => ({
    entityId: state.selectedAccountId,
    recipient: {
      firstName: formData.get('transfer-recipient-first-name'),
      lastName: formData.get('transfer-recipient-last-name'),
      identification: formData.get('transfer-recipient-account-number'),
      accountEnvironment: parseInt(formData.get('transfer-recipient-account-environment')),
      identificationStrategy: 0,
      currency: 'USD',
      routingNumber: null
    }
  }),
  resetDomAfterRecipientRegistration
)

function listenForFormSubmit (formEl, url, formDataToProps, postSubmit) {
  formEl.addEventListener('submit', e => {
    e.preventDefault()

    jsonPost(url, formDataToProps(new FormData(formEl)))
      .then(_ => {
        closeFormView(formEl)
        if (typeof postSubmit === 'function') return postSubmit()
        renderAccountActionList()
      })
  })
}

function getTransferRecipient (recipientId) {
  return state.transferRecipients[recipientId]
}

function jsonPost (url, props) {
  return fetch(url, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(props)
  })
  .then(res => res.json())
  .then(res => {
    if (res.validationError) {
      openValidationErrorModal(res.validationError)
      return
    }
    return res
  })
  .catch(notifyError)
}

function notifyError (err) {
  const errMsg = err.toString()
  console.error(errMsg)
  alert(errMsg)
}

// Validation Error Modal Logic

const modal = {
  classes: {
    open: 'modal-is-open',
    opening: 'modal-is-opening',
    closing: 'modal-is-closing'
  },
  animationDuration: 300
}

const openValidationErrorModal = errorReason => {
  selectors.validationErrorReason().textContent = errorReason
  document
    .documentElement
    .classList
    .add(modal.classes.open, modal.classes.opening)

  selectors.validationErrorModal().setAttribute('open', true)

  setTimeout(() => {
    state.isValidationErrorModalOpen = true
    document.documentElement.classList.remove(modal.classes.opening)
  }, modal.animationDuration)
}

const closeValidationErrorModal = () => {
  state.isValidationErrorModalOpen = false
  document.documentElement.classList.add(modal.classes.closing)

  setTimeout(() => {
    document
      .documentElement
      .classList
      .remove(modal.classes.closing, modal.classes.open)

    selectors.validationErrorModal().removeAttribute('open')
  }, modal.animationDuration)
}

// Close with a click outside modal
document.addEventListener('click', e => {
  if (state.isValidationErrorModalOpen) {
    const modalContent = selectors.validationErrorModal().querySelector('article')
    const isClickInside = modalContent.contains(e.target)
    !isClickInside && closeValidationErrorModal()
  }
})

// Close with Esc key
document.addEventListener('keydown', e => {
  if (e.key === 'Escape' && state.isValidationErrorModalOpen) {
    closeValidationErrorModal()
  }
})
