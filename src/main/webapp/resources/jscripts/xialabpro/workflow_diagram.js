/* 
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/ClientSide/javascript.js to edit this template
 */


$(document).ready(function () {
    setTimeout(() => {
        // Attach click event to all diagram elements
        attachEvent();
        attachEdgeEvent();
    }, 500); // Ensure diagram is rendered before attaching events
});

function attachEvent() {
    $('.ui-diagram-element').on('click', function () {
        // Remove 'selected' class from all elements
        $('.ui-diagram-element').removeClass('selected');

        // Add 'selected' class to the clicked element
        //$(this).addClass('selected');

        // Retrieve the tooltip (or title) of the clicked element
        const tooltip = $(this).data('tooltip');
        //console.log($(this))
        if (tooltip) {
            handleElementClick([{name: 'clickedElement', value: tooltip}]);
        }

    });

    $('.ui-diagram-element').on('dblclick', function () {
        // Remove 'selected' class from all elements
        $('.ui-diagram-element').removeClass('selected');

        // Add 'selected' class to the clicked element
        $(this).addClass('selected');

        // Retrieve the tooltip (or title) of the clicked element
        const tooltip = $(this).data('tooltip');
        if (tooltip) {
            handleElementDoubleClick([{name: 'clickedElement', value: tooltip}]);
        }

    });

}

function attachEdgeEvent() {
    const connections = document.querySelectorAll(".jtk-connector");
    console.log(connections);
    connections.forEach((connection) => {
        connection.addEventListener("dblclick", function () {
            // Highlight the clicked connection
            connections.forEach(conn => conn.classList.remove("highlight"));
            this.classList.add("highlight");
            const sourceId = this["_jsPlumb"]["endpoints"][0]["_jsPlumb"]["uuid"];
            const targetId = this["_jsPlumb"]["endpoints"][1]["_jsPlumb"]["uuid"];
            console.log("Connection clicked:", sourceId, targetId);

            // Call remote command or handle connection click
            removeConnection([{name: "sourceId", value: sourceId}, {name: "targetId", value: targetId}]);
        });
    });
}

function reattachEvents() {
    attachEvent();
    attachEdgeEvent();
}

function startNavigation(pagesToVisit) {
    console.log("pages=============");
    localStorage.setItem('pagesToVisit', JSON.stringify(pagesToVisit));
    // Navigate to the first page after a short delay
    console.log("pagesToVisit = " + pagesToVisit)
    PF('growlWidget').show([{severity: "info", summary: "INFO", detail: "Execution Finished! Now fetching result images... Please be patient."}]);

    setTimeout(function () {
        window.location.href = pagesToVisit[0];
    }, 1500); // Delay before navigating to the first page
}
