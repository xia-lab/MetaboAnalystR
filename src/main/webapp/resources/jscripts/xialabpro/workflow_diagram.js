// === Diagram event wiring for PrimeFaces <p:diagram widgetVar="networkDiagram"> ===

// Single-vs-double click guard
let __clickTimer = null;
const __CLICK_DELAY = 220; // ms

// Track which root DOM nodes we’ve already bound to, so we don’t double-bind
const __boundRoots = new WeakSet();

/** Resolve the current diagram root jQuery collection. */
function getDiagramRoot$() {
    // PF('networkDiagram').jq is the root jQuery element for the widget
    try {
        const w = PF('networkDiagram');
        if (w && w.jq && w.jq.length)
            return w.jq;
    } catch (e) {
    }
    // Fallback: if you know an explicit container ID, you can return $('#diagramContainer');
    return $(); // empty -> caller will no-op
}

/** Bind delegated handlers to the current root (idempotent per DOM node). */
function bindHandlersToCurrentRoot() {
    const $root = getDiagramRoot$();
    if (!$root.length)
        return;

    const rootEl = $root.get(0);
    if (__boundRoots.has(rootEl))
        return; // already bound

    // --- Nodes (delegated) ---
    // Remove any lingering namespaced handlers on this root before attaching
    $root.off('click.ui-diagram-element dblclick.ui-diagram-element');

    $root.on('click.ui-diagram-element', '.ui-diagram-element', function () {
        clearTimeout(__clickTimer);
        const $el = $(this);
        const tooltip = $el.data('tooltip');

        __clickTimer = setTimeout(() => {
            // Single-click behavior
            $('.ui-diagram-element').removeClass('selected');
            // keep selection off for single-click as in your original code
            if (tooltip) {
                // Provided by your app
                handleElementClick([{name: 'clickedElement', value: tooltip}]);
            }
        }, __CLICK_DELAY);
    });

    $root.on('dblclick.ui-diagram-element', '.ui-diagram-element', function () {
        clearTimeout(__clickTimer);
        const $el = $(this);
        const tooltip = $el.data('tooltip');

        $('.ui-diagram-element').removeClass('selected');
        $el.addClass('selected');

        if (tooltip) {
            // Provided by your app
            handleElementDoubleClick([{name: 'clickedElement', value: tooltip}]);
        }
    });

    // --- Edges (delegated) ---
    $root.off('dblclick.jtk-connector');
    $root.on('dblclick.jtk-connector', '.jtk-connector', function () {
        // Highlight this connection
        $root.find('.jtk-connector').removeClass('highlight');
        this.classList.add('highlight');

        // Prefer data-* if you can populate them from jsPlumb; else guarded fallback
        let sourceId = this.dataset.sourceId;
        let targetId = this.dataset.targetId;

        if (!sourceId || !targetId) {
            try {
                const endpoints = this['_jsPlumb']?.['endpoints'];
                if (endpoints && endpoints[0] && endpoints[1]) {
                    sourceId = endpoints[0]['_jsPlumb']?.['uuid'];
                    targetId = endpoints[1]['_jsPlumb']?.['uuid'];
                }
            } catch (err) {
                console.warn('Could not resolve source/target from connector', err);
            }
        }

        if (sourceId && targetId) {
            // Provided by your app
            removeConnection([
                {name: 'sourceId', value: sourceId},
                {name: 'targetId', value: targetId}
            ]);
        }
    });

    __boundRoots.add(rootEl);
}

/** Call this if you explicitly need to refresh bindings (rare with delegation). */
function reattachEvents() {
    // If PF replaced the DOM root on update, we need to bind to the new one.
    bindHandlersToCurrentRoot();
}

/** Optional: If you have access to jsPlumb instance, stamp data-* on the canvas for safer reads. */
function enhanceJsPlumbBindings(instance) {
    if (!instance)
        return;
    instance.bind('connection', function (info) {
        const canvas = info.connection?.canvas;
        if (canvas) {
            canvas.dataset.sourceId = info.sourceId;
            canvas.dataset.targetId = info.targetId;
        }
        // Also attach a dblclick directly on the connection (redundant but robust)
        info.connection?.bind?.('dblclick', function () {
            removeConnection([
                {name: 'sourceId', value: info.sourceId},
                {name: 'targetId', value: info.targetId}
            ]);
        });
    });
}

/** Keep your navigation helper as-is */
function startNavigation(pagesToVisit) {
    localStorage.setItem('pagesToVisit', JSON.stringify(pagesToVisit));
    PF('growlWidget').show([{
            severity: 'info',
            summary: 'INFO',
            detail: 'Execution Finished! Now fetching result images... Please be patient.'
        }]);
    setTimeout(function () {
        window.location.href = pagesToVisit[0];
    }, 1500);
}

(function boot(init) {
  // If PF is already there and exposes onReady, use it.
  if (window.PrimeFaces && typeof PrimeFaces.onReady === 'function') {
    PrimeFaces.onReady(init);
    return;
  }

  // Fallback: run on DOM ready (covers cases where PF loads later or is missing onReady)
  if (window.jQuery) {
    jQuery(init);
    jQuery(window).on('load', init);
  } else {
    // ultra-fallback if jQuery is not yet available for some reason
    if (document.readyState === 'loading') {
      document.addEventListener('DOMContentLoaded', init, { once: true });
    } else {
      init();
    }
    window.addEventListener('load', init, { once: true });
  }

  // If PF shows up later, bind again (idempotent)
  const interval = setInterval(() => {
    if (window.PrimeFaces && typeof PrimeFaces.onReady === 'function') {
      clearInterval(interval);
      PrimeFaces.onReady(init);
    }
  }, 100);
})(function () {
  // your existing entrypoint:
  bindHandlersToCurrentRoot();
});

// Rebind after partial updates (diagram re-renders)
(function hookPrimeFacesAjax() {
    // PF 10/11+ Queue API
    const addOnComplete = PrimeFaces.ajax?.Queue?.addOnComplete || PrimeFaces.ajax?.addOncomplete;
    if (addOnComplete) {
        addOnComplete(function () {
            // Root may be replaced; bind to the new root if needed
            bindHandlersToCurrentRoot();
        });
    } else {
        // Very old fallback
        $(document).on('pfAjaxComplete', bindHandlersToCurrentRoot);
    }
})();

window.onload = function () {
    PF("workflowProgressDialog").hide();
    localStorage.removeItem('pagesToVisit');

    var reportEndBool = localStorage.getItem('reportEndBool');
    if (reportEndBool) {
        PF('growlWidget').show([{severity: "info", summary: "INFO", detail: "Execution Finished! Click 'Dashboard' button to view results."}]);
        localStorage.removeItem('reportEndBool');

    }

    //<![CDATA[
    function getURLParameter(name) {
        return decodeURIComponent(
                (new RegExp('[?|&]' + name + '=' + '([^&;]+?)(&|#|;|$)').exec(location.search) || [null, ''])[1].replace(/\+/g, '%20')
                ) || null;
    }
    //]]>
    /*
     if (getURLParameter('callFunc') !== null) {
     if (getURLParameter('callFunc') === "checkPagesToVisit") {
     checkPagesToVisit([]);
     }
     }
     */
};