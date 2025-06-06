
var sigInst, settings, vizMode;

sigma.utils.pkg('sigma.canvas.nodes');
sigma.canvas.nodes.def = function (node, context, settings) {
    var prefix = settings('prefix') || '';

    context.fillStyle = node.color || settings('defaultNodeColor');
    context.beginPath();
    context.arc(
            node[prefix + 'x'],
            node[prefix + 'y'],
            node[prefix + 'size'],
            0,
            Math.PI * 2,
            true
            );

    context.closePath();
    context.fill();

    // Adding a border
    context.lineWidth = 2;
    //alert("called here!");
    //context.strokeStyle = node.borderColor || '#ccc';
    context.strokeStyle = 'grey';
    context.stroke();
};

function showSigmaNetwork() {
    vizMode = "other";
    sigInst = new sigma({
        renderers: [
            {
                container: document.getElementById('sigma-example'),
                type: 'canvas' // sigma.renderers.canvas works as well
            }
        ],
        settings: {
            defaultLabelColor: '#000',
            defaultLabelSize: 12,
            defaultLabelBGColor: '#fff',
            defaultLabelHoverColor: '#000',
            labelThreshold: 8,
            defaultEdgeColor: '#ccc',
            defaultEdgeLabelColor: '#fff',
            defaultEdgeLabelSize: 10,
            defaultEdgeLabelBGColor: '#fff',
            defaultEdgeLabelHoverColor: '#fff',
            edgeLabelThreshold: 4,
            labelAlignment: 'right',
            minEdgeSize: 1,
            maxEdgeSize: 1,
            enableEdgeHovering: true,
            edgeHoverColor: 'edge',
            minNodeSize: 4,
            maxNodeSize: 20,
            sideMargin: 12,
            borderSize: 0,
            doubleClickEnabled: false,
            nodeBorderColor: "white"
        }
    });
    sigma.plugins.dragNodes(sigInst, sigInst.renderers[0]);
    sigInst.bind('doubleClickNode', function (e) {
        console.log(e);
        PF('msetDialog').hide();
        $.ajax({
            dataType: "html",
            type: "POST",
            url: '/MetaboAnalyst/faces/AjaxCall',
            data: {function: 'prepareMsetView', id: e.data.node.id},
            async: false,
            cache: false,
            success: function () {
                updateMset();
            },
            error: function () {
                $.messager.alert('Error', 'Failed to process the request!', 'error');
            }
        });
    });

    var config = {
        nodeMargin: 8.0,
        scaleNodes: 1.3
    };

// Configure the algorithm
    sigInst.configNoverlap(config);
    $.getJSON('/MetaboAnalyst' + document.getElementById("mydir").value + "/msea_network.json",
            function (data) {
                netData = data;
                for (var i = 0; i < data.nodes.length; i++) {
                    var nd = data.nodes[i];
                    nd.x = parseInt(nd.x);
                    nd.y = parseInt(nd.y);
                    sigInst.graph.addNode(nd);
                }
                for (var j = 0; j < data.edges.length; j++) {
                    var eg = data.edges[j];
                    eg.color = "#ccc";
                    if (!(eg.source === undefined || eg.target === undefined)) {
                        sigInst.graph.addEdge(eg);
                    }
                }
                sigInst.refresh();
                sigInst.startNoverlap();//adjust time for laying out, 
            });
        initReportFunctions();

        setTimeout(function () {
            checkSavedState("report_msea_network.json");
        }, 1000);
    
}

function export2SVG() {
    sigInst.toSVG({download: true, filename: 'msea_network.svg', labels: true, background: "#ffffff"});
}
