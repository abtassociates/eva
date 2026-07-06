// 1. Hide any potential disconnect overlays
function hide_disconnect() {
  var style = document.createElement('style');
  style.id = 'ss-hide-overlay-style';
  style.innerHTML = '#ss-overlay, #ss-connect-dialog { display: none !important; }';
  document.head.appendChild(style);
}

// AUTH STATE
var uploaded_file = null;
var timeout = null;
var idleTimeout = 1000 * 60 * 15; //15 minutes

// OVERLAY
function showTimeoutOverlay() {
  document.getElementById('timeout-overlay').style.display = 'flex';
}

// TIMER LOGIC (GATED)
function resetTimer() {
  if (uploaded_file !== true) return;
  clearTimeout(timeout);
  timeout = setTimeout(function() {
    hide_disconnect();
    showTimeoutOverlay();
  }, idleTimeout);
}

// ACTIVITY HANDLER
function markActivity() {
  if (uploaded_file !== true) return;
  resetTimer();
}

// SHINY AUTH MESSAGE
Shiny.addCustomMessageHandler('uploaded_file', function(l) {
  uploaded_file = l;
  if (uploaded_file === true) resetTimer(); 
  document.getElementById('ss-hide-overlay-style')?.remove();
});

// USER ACTIVITY EVENTS
var events = ['click', 'mousemove', 'keypress', 'scroll'];

events.forEach(function(e) {
  document.addEventListener(e, markActivity, { passive: true });
});