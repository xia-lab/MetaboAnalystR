// resources/jscripts/xialabpro/ws_handler.js
// WebSocket-based workflow status handler (replaces SSE)

(function () {
  if (!window.__wfWs) {
    window.__wfWs = { ws: null, initOnce: false, reconnectAttempts: 0 };
  }
  const S = window.__wfWs;

  function wsUrl() {
    // Get context path - try multiple methods
    var ctx = window.__appCtx || "";

    // Fallback: extract from current page URL
    if (!ctx) {
      var path = window.location.pathname;
      //console.log("[WS] Current pathname:", path);

      // Look for /MetaboAnalyst specifically, or extract before /Secure or /resources
      if (path.indexOf('/MetaboAnalyst') === 0) {
        ctx = '/MetaboAnalyst';
      } else if (path.indexOf('/Secure') === 0 || path.indexOf('/resources') === 0) {
        // These are app paths, context is likely /MetaboAnalyst
        ctx = '/MetaboAnalyst';
      } else {
        // Try to extract first segment that's not a known app folder
        var match = path.match(/^(\/[^\/]+)/);
        if (match && match[1] !== "/" && !match[1].match(/\.(xhtml|html|jsp)$/)) {
          // Check if it's a known app folder, if so, assume /MetaboAnalyst context
          if (match[1] === '/Secure' || match[1] === '/resources' || match[1] === '/faces') {
            ctx = '/MetaboAnalyst';
          } else {
            ctx = match[1];
          }
        }
      }
    }

    // Fallback: check this script's own URL (loaded from /MetaboAnalyst/resources/...)
    if (!ctx) {
      var scripts = document.getElementsByTagName('script');
      for (var i = 0; i < scripts.length; i++) {
        var src = scripts[i].src;
        if (src && src.indexOf('ws_handler.js') > -1) {
          //console.log("[WS] Found script URL:", src);
          // Extract context from script URL like "http://host/MetaboAnalyst/resources/..."
          var match = src.match(/\/\/[^\/]+(\/[^\/]+)\/resources/);
          if (match) {
            ctx = match[1];
            //console.log("[WS] Extracted context from script URL:", ctx);
          }
          break;
        }
      }
    }

    // Fallback: try JSF context from form action
    if (!ctx) {
      var contextInput = document.querySelector('input[name="javax.faces.ViewState"]');
      if (contextInput && contextInput.form) {
        var action = contextInput.form.action;
        if (action) {
          var match = action.match(/^https?:\/\/[^\/]+(\/[^\/]+)/);
          if (match) ctx = match[1];
        }
      }
    }

    //console.log("[WS] Final detected context path:", ctx || "(root)");

    var protocol = window.location.protocol === "https:" ? "wss:" : "ws:";
    var host = window.location.host;

    // Get userId - try multiple sources
    var userId = window.__userId ||
                 (window.fireUserBean && window.fireUserBean.email) ||
                 getUserIdFromPage();

    if (!userId) {
      console.warn("[WS] No userId found, connection will fail");
      userId = "unknown";
    }

    // URL encode the userId
    userId = encodeURIComponent(userId);

    return protocol + "//" + host + ctx + "/workflow/ws/" + userId;
  }

  function getUserIdFromPage() {
    // Try to get from hidden input or meta tag
    var input = document.getElementById("userId") || document.querySelector("[name='userId']");
    if (input) return input.value;

    var meta = document.querySelector("meta[name='userId']");
    if (meta) return meta.content;

    return null;
  }

  function connect() {
    // Don't reconnect if already open or connecting
    if (S.ws && (S.ws.readyState === WebSocket.OPEN || S.ws.readyState === WebSocket.CONNECTING)) {
      return;
    }

    // Clean up old connection
    try {
      if (S.ws) S.ws.close();
    } catch (e) {}

    const url = wsUrl();
    //console.log("[WS] Connecting:", url);

    try {
      const ws = new WebSocket(url);
      S.ws = ws;

      ws.onopen = function () {
        //console.log("[WS] ========== CONNECTION OPENED ==========");
        //console.log("[WS] Session ID:", session);
        //console.log("[WS] Ready state:", ws.readyState);
        S.reconnectAttempts = 0;
      };

      ws.onmessage = function (event) {
        //console.log("[WS] ========== MESSAGE RECEIVED ==========");
        //console.log("[WS] Raw data:", event.data);
        //console.log("[WS] Data type:", typeof event.data);
        //console.log("[WS] Data length:", event.data ? event.data.length : 0);

        if (!event.data || event.data.length === 0) {
          console.warn("[WS] Empty message received!");
          return;
        }

        try {
          const msg = JSON.parse(event.data);
          //console.log("[WS] Parsed JSON:", msg);
          //console.log("[WS] Message type:", msg.type);

          // Handle different message types
          if (msg.type === "connected") {
            //console.log("[WS] Connection confirmed:", msg.message);
          } else if (msg.type === "ping") {
            //console.log("[WS] Received PING from server");
            // Heartbeat from server - just update timestamp
            return;
          } else if (msg.type === "pong") {
            //console.log("[WS] Received PONG from server");
            // Response to our ping
            return;
          } else if (msg.type === "workflow-status" && msg.data) {
            //console.log("[WS] Workflow update (wrapped):", msg.data);
            // Workflow update with wrapped data
            handleWorkflowUpdate(msg.data);
          } else if (msg.runId && msg.status) {
            //console.log("[WS] Workflow update (direct):", msg);
            // Direct workflow update (not wrapped)
            handleWorkflowUpdate(msg);
          } else {
            //console.log("[WS] Unknown message type:", msg);
          }
        } catch (err) {
          console.error("[WS] JSON parse error:", err);
          console.error("[WS] Failed to parse:", event.data);
        }
      };

      ws.onerror = function (error) {
        console.error("[WS] Error:", error);
      };

      ws.onclose = function (event) {
        console.warn("[WS] Closed (code: " + event.code + ", reason: " + event.reason + ")");
        S.ws = null;

        // Reconnect with exponential backoff
        S.reconnectAttempts++;
        const delay = Math.min(1000 * Math.pow(2, S.reconnectAttempts - 1), 30000);
        //console.log("[WS] Reconnecting in " + (delay / 1000) + "s (attempt " + S.reconnectAttempts + ")");
        setTimeout(connect, delay);
      };
    } catch (e) {
      console.error("[WS] Failed to create WebSocket:", e);
      setTimeout(connect, 5000);
    }
  }

  function reconnect() {
    try {
      if (S.ws) S.ws.close();
    } catch (e) {}
    S.ws = null;
    setTimeout(connect, 300 + Math.random() * 700);
  }

  function handleWorkflowUpdate(data) {
    //console.log("[WS] Workflow update:", data);

    if (typeof updateWorkflowRunUI === "function") {
      updateWorkflowRunUI(data.runId, data.status, data.projectId);
    }
  }

  // Public API
  window.ensureWorkflowStream = function () {
    connect();
  };

  // Test function - call from console: testWebSocket()
  window.testWebSocket = function() {
    if (!S.ws || S.ws.readyState !== WebSocket.OPEN) {
      console.error("[WS TEST] WebSocket not connected!");
      return;
    }
    //console.log("[WS TEST] Sending test message...");
    S.ws.send('test');
  };

  // Cleanup on page unload
  window.addEventListener("beforeunload", function () {
    try {
      if (S.ws) S.ws.close();
    } catch (e) {}
    S.ws = null;
  });

  // Reconnect when page becomes visible again
  window.addEventListener("pageshow", function () {
    window.ensureWorkflowStream();
  });

  // Reconnect after AJAX calls (PrimeFaces integration)
  if (window.PF && PF.ajax && PF.ajax.AddOnComplete) {
    PF.ajax.AddOnComplete(function () {
      window.ensureWorkflowStream();
    });
  }

  // Auto-start on page load
  if (!S.initOnce) {
    S.initOnce = true;
    window.addEventListener("load", function () {
      window.ensureWorkflowStream();
    });
  }

  // UI update function - refreshes table from server
  function updateWorkflowRunUI(runId, status, projectId) {
    //console.log('[WS] Updating UI for run #' + runId + ' to status: ' + status + ', projectId: ' + projectId);

    // Refresh the table
    if (typeof refreshWorkflowRunTable === 'function') {
      refreshWorkflowRunTable();
    }

    // Show notification
    if (typeof PF === 'function' && PF('growlWidget')) {
      //console.log('[WS] Showing growl notification for status: ' + status);

      if (status === 'completed') {
        PF('growlWidget').show([{
          severity: 'info',
          summary: 'Workflow Complete',
          detail: 'Run #' + runId + ' has finished successfully.',
          sticky: false
        }]);
      } else if (status === 'failed') {
        PF('growlWidget').show([{
          severity: 'error',
          summary: 'Workflow Failed',
          detail: 'Run #' + runId + ' encountered an error.',
          sticky: true
        }]);
      } else if (status === 'running') {
        PF('growlWidget').show([{
          severity: 'info',
          summary: 'Workflow Running',
          detail: 'Run #' + runId + ' is now processing...',
          sticky: false
        }]);
      }
    } else {
      console.warn('[WS] PrimeFaces growlWidget not available');
    }
  }

})();
