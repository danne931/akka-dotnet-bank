'use strict'

const connection = new signalR.HubConnectionBuilder()
  .withUrl('/accountHub')
  .build()

connection.on('ReceiveError', function (err) {
  const msg = `Error: ${err}`
  console.log(msg)
  alert(msg)
})

connection.on('ReceiveMessage', function (stateTransition) {
  interpolateEventIntoListView(stateTransition.event)
})

const accountId = 'ec3e94cc-eba1-4ff4-b3dc-55010ecf67e9'
connection
  .start()
  .then(() => connection.invoke('AddToConnectionGroup', accountId))
  .then(() => fetch(`/diagnostic/events/${accountId}`))
  .then(res => {
    if (!res.ok) {
      throw new Error(`Http status: ${res.status}`)
    }
    return res.json();
  })
  .then(events => {
    events.forEach(interpolateEventIntoListView);
  })
  .catch(err => console.error(err.toString()))

function interpolateEventIntoListView (evt) {
  const li = document.createElement('li')
  document.getElementById('messagesList').appendChild(li)
  li.textContent = eventToTransactionString(evt)
}

function eventToTransactionString (evt) {
  return {
    'CreatedAccount': `Created account for ${evt.firstName} ${evt.lastName} on ${evt.timestamp}`,
    'DepositedCash': `$${evt.depositedAmount} deposited on ${evt.timestamp}`,
    'DebitedAccount': `$${evt.debitedAmount} debited from account by ${evt.origin} on ${evt.timestamp}`
  }[evt.name]
}