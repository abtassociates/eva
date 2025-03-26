function idleTimer() {
  var timeoutTime = 5 * 1000; //1000 = 1 second
  var t = setTimeout(showTimeoutDialog, timeoutTime);
  // Store references to the event listener functions so we can remove them later
  var resetTimerHandlers = {
    mousemove: function() { resetTimer(); },
    mousedown: function() { resetTimer(); },
    click: function() { resetTimer(); },
    scroll: function() { resetTimer(); },
    keypress: function() { resetTimer(); }
  };

  window.addEventListener("mousemove", resetTimerHandlers.mousemove);
  window.addEventListener("mousedown", resetTimerHandlers.mousedown);
  window.addEventListener("click", resetTimerHandlers.click);
  window.addEventListener("scroll", resetTimerHandlers.scroll);
  window.addEventListener("keypress", resetTimerHandlers.keypress);
  
  function showTimeoutDialog() {
    // Remove event listeners and clear the timeout to prevent repeated calls
    window.removeEventListener("mousemove", resetTimerHandlers.mousemove);
    window.removeEventListener("mousedown", resetTimerHandlers.mousedown);
    window.removeEventListener("click", resetTimerHandlers.click);
    window.removeEventListener("scroll", resetTimerHandlers.scroll);
    window.removeEventListener("keypress", resetTimerHandlers.keypress);
    clearTimeout(t);
    
    document.querySelector('a[data-value=\"tabHome\"]').click();
    
    // Add a banner at the top of the page
    var banner = document.createElement('div');
    banner.style.position = 'fixed';
    banner.style.top = '40%';
    banner.style.left = '0';
    banner.style.width = '100%';
    banner.style.padding = '10px';
    banner.style.backgroundColor = 'lightgray';
    banner.style.color = 'black';
    banner.style.fontSize = '2em';
    banner.style.textAlign = 'center';
    banner.style.zIndex = '9999';
    banner.innerHTML = 'Session ended due to inactivity. Please refresh the page to continue.';
    document.body.appendChild(banner);
    
    // **MutationObserver to hide the dialog when it appears**
    const observer = new MutationObserver(function(mutationsList, observer) {
      for (let mutation of mutationsList) {
        if (mutation.type === 'childList') {
          if ($('#ss-connect-dialog').length > 0) {
            $('#ss-connect-dialog').hide();
            observer.disconnect(); // Stop observing after hiding it
            break; // Exit the loop after hiding it
          }
        }
      }
    });
    
    // Start observing the document body for added nodes
    observer.observe(document.body, { childList: true, subtree: true });

    Shiny.setInputValue('session_idle', true, {priority: 'event'});
  }

  function resetTimer() {
      clearTimeout(t);
      t = setTimeout(showTimeoutDialog, timeoutTime);
  }
}
idleTimer();