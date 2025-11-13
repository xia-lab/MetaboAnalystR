// Simple polling solution for workflow status updates
// Much simpler than WebSocket and works everywhere

(function() {
  let pollTimer = null;
  let isPolling = false;

  let POLL_INTERVAL = 10000; // Poll every 10 seconds (adjustable)

  function startPolling() {
    if (isPolling) {
      return;
    }

    console.log('[POLLER] Starting workflow status polling (every ' + (POLL_INTERVAL / 1000) + 's)');
    isPolling = true;

    // Poll immediately on start
    pollStatus();

    // Then poll at regular intervals
    pollTimer = setInterval(pollStatus, POLL_INTERVAL);
  }

  function stopPolling() {
    if (!isPolling) {
      return;
    }

    console.log('[POLLER] Stopped workflow status polling');
    isPolling = false;

    if (pollTimer) {
      clearInterval(pollTimer);
      pollTimer = null;
    }
  }

  function pollStatus() {
    // Call the PrimeFaces remote command to refresh the table
    if (typeof refreshWorkflowRunTable === 'function') {
      refreshWorkflowRunTable();
    } else if (typeof window.refreshWorkflowRunTable === 'function') {
      window.refreshWorkflowRunTable();
    } else {
      console.warn('[POLLER] refreshWorkflowRunTable function not available');
    }
  }

  // Public API
  window.workflowStatusPoller = {
    start: startPolling,
    stop: stopPolling,
    isActive: function() { return isPolling; },
    setInterval: function(ms) {
      const wasPolling = isPolling;
      stopPolling();
      POLL_INTERVAL = ms;
      if (wasPolling) {
        startPolling();
      }
      console.log('[POLLER] Interval updated to ' + (ms / 1000) + 's');
    }
  };

  // Auto-start on page load if we're on the Workflow Projects tab
  window.addEventListener('load', function() {
    // Check if we're on the ProjectView page
    if (window.location.pathname.includes('ProjectView.xhtml')) {
      // Start polling after a short delay to ensure page is ready
      setTimeout(function() {
        // Check if we're on the Workflow Projects tab (index 1)
        if (typeof PF === 'function' && PF('tabWidget')) {
          var activeIndex = PF('tabWidget').getActiveIndex();
          if (activeIndex === 1) {
            startPolling();
          }
        } else {
          // Fallback: just start polling
          startPolling();
        }
      }, 2000);
    }
  });

  // Stop polling when page unloads
  window.addEventListener('beforeunload', function() {
    stopPolling();
  });

  // Stop/start polling when page visibility changes (browser optimization)
  document.addEventListener('visibilitychange', function() {
    if (document.hidden) {
      stopPolling();
    } else if (window.location.pathname.includes('ProjectView.xhtml')) {
      // Only resume if we're on the Workflow Projects tab
      if (typeof PF === 'function' && PF('tabWidget')) {
        var activeIndex = PF('tabWidget').getActiveIndex();
        if (activeIndex === 1) {
          startPolling();
        }
      } else {
        // Fallback: resume polling
        startPolling();
      }
    }
  });

})();
