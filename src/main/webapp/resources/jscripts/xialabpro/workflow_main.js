var cy;
var selArray = [];
var customMode = false;
var navUrl;
document.addEventListener("DOMContentLoaded", function () {
    initNetwork();
});

function initNetwork() {
    var analType = document.getElementById('formHidden1:naviType').value;
    var analType2 = document.getElementById('formHidden1:analType').value;
    //console.log("analType2====" + analType2);
    if (analType === "unknown" || analType === "NA") {
        analType = "stat";
    }
    if (analType === "") {
        analType = analType2;
    }
    // Ensure both Cytoscape and cytoscape-dagre are loaded
    function waitForCytoscape(callback) {
        if (typeof cytoscape !== 'undefined' && typeof cytoscapeDagre !== 'undefined') {
            callback();
        } else {
            setTimeout(function () {
                waitForCytoscape(callback);
            }, 100);
        }
    }


    waitForCytoscape(function () {
        // Register the cytoscape-dagre extension
        cytoscape.use(cytoscapeDagre);
        var netType = "";
        if (["msetssp", "msetora", "msetqea", "enrich-ssp", "enrich-ora", "enrich-qea"].indexOf(analType) !== -1) {
            netType = "enrich";
        } else if (["pathora", "pathqea", "pathway-qea"].indexOf(analType) !== -1) {
            netType = "path";
        } else if (["dspc", "dspc-peak", "network"].indexOf(analType) !== -1) {
            netType = "network";
        } else if (["mummichog-table"].indexOf(analType) !== -1) {
            netType = "mummichog";
        } else {
            netType = analType;
        }
        var fileName;
        //console.log(netType + " ====netype");
        if (netType === "stat" || netType === "mf") {
            fileName = netType + "_net_v2.json";
        } else {
            fileName = netType + "_net.json";
        }

        fetch("/MetaboAnalyst/resources/pro/" + fileName)
                .then(response => response.json())
                .then(data => {

                    data.elements.nodes.forEach(node => {
                        if (!node.data.backgroundOpacity) {
                            node.data.backgroundOpacity = 0.5;
                            node.data.transparent = true;
                        }
                    });

                    cy = cytoscape({
                        container: document.getElementById('sigma-example'), // container to render in

                        style: [// the stylesheet for the graph
                            {
                                selector: 'node',
                                style: {
                                    'shape': 'round-rectangle', // set the shape to rectangle with rounded corners
                                    'background-color': 'data(color)',
                                    'background-opacity': 'data(backgroundOpacity)',
                                    'transparent': 'true',
                                    'label': 'data(label)',
                                    'text-valign': 'center',
                                    'text-halign': 'center',
                                    'color': '#fff',
                                    'width': 'label', // auto-size width to fit the label
                                    'height': 'label', // auto-size height to fit the label
                                    'padding-left': '5px',
                                    'padding-right': '5px',
                                    'padding-top': '5px',
                                    'padding-bottom': '5px',
                                    'font-size': '25px'
                                }
                            },

                            {
                                selector: '$node > node', // compound nodes
                                style: {
                                    'shape': 'round-rectangle',
                                    'background-color': 'data(color)',
                                    'background-opacity': 0.3,
                                    'label': '',
                                    'text-valign': 'top',
                                    'text-halign': 'center',
                                    'border-width': 2,
                                    'border-color': '#666',
                                    'color': '#666',
                                    'font-size': '14px',
                                    'padding-left': '20px', // Set padding to make compound nodes larger
                                    'padding-right': '20px', // Set padding to make compound nodes larger
                                    'padding-top': '20px', // Set padding to make compound nodes larger
                                    'padding-bottom': '20px' // Set padding to make compound nodes larger
                                }
                            },
                            {
                                selector: 'edge',
                                style: {
                                    'width': 3,
                                    'line-color': '#ccc',
                                    'target-arrow-color': '#ccc',
                                    'target-arrow-shape': 'triangle',
                                    'curve-style': 'bezier'
                                }
                            },
                            {
                                selector: '.highlighted',
                                style: {
                                    'background-color': '#61bffc',
                                    'line-color': '#61bffc',
                                    'target-arrow-color': '#61bffc',
                                    'transition-property': 'background-color, line-color, target-arrow-color',
                                    'transition-duration': '0.5s',
                                },
                            },
                            {
                                selector: '.highlighted-green-infinite',
                                style: {
                                    'background-color': '#00FF00',
                                    'line-color': '#00FF00',
                                    'target-arrow-color': '#00FF00',
                                    'transition-property': 'background-color, line-color, target-arrow-color',
                                    'transition-duration': '0.5s',
                                },
                            },

                            {
                                selector: '.highlighted-green',
                                style: {
                                    'background-color': '#00FF00',
                                    'line-color': '#00FF00',
                                    'target-arrow-color': '#00FF00',
                                    'transition-property': 'background-color, line-color, target-arrow-color',
                                    'transition-duration': '0.5s',
                                },
                            }
                            ,

                            {
                                selector: '.highlighted-orange',
                                style: {
                                    'background-color': '#FF5733 ',
                                    'line-color': '#FF5733 ',
                                    'target-arrow-color': '#FF5733 ',
                                    'transition-property': 'background-color, line-color, target-arrow-color',
                                    'transition-duration': '0.5s',
                                },
                            },

                            {
                                selector: '.highlighted-red',
                                style: {
                                    'background-color': '#FF0000 ',
                                    'line-color': '#FF0000 ',
                                    'target-arrow-color': '#FF0000 ',
                                    'transition-property': 'background-color, line-color, target-arrow-color',
                                    'transition-duration': '0.5s',
                                },
                            }
                        ]
                    });
                    if (!fileName.includes("_v2")) {
                        cy.ready(function () {

                            cy.add(data.elements);

                            const longestPath = calculateLongestPath(cy) || 5; // Default to 1 if empty or undefined
                            const targetHeight = 400;
                            const baseRankSep = 30;
                            const adjustedRankSep = Math.max(baseRankSep, targetHeight / longestPath);

                           // console.log('Longest Path:', longestPath);
                            //console.log('Adjusted RankSep:', adjustedRankSep);

                            if (cy.nodes().length > 0 && cy.edges().length > 0) {
                                cy.layout({
                                    name: 'dagre',
                                    rankDir: 'TB',
                                    nodeSep: 20,
                                    edgeSep: 10,
                                    rankSep: adjustedRankSep > 0 ? adjustedRankSep : 50 // Ensure valid rankSep
                                }).run();
                            } else {
                                console.warn('The graph is empty or disconnected. Layout will not be applied.');
                            }
                        });
                    } else {
                        cy.json(data);
                    }
                    cy.style()
                            .selector('#Start')
                            .style({
                                'shape': 'ellipse',
                                'label': 'Start ▶',
                                'background-color': '#61bffc',
                                'height': '75'
                            })
                            .update();
                    cy.style()
                            .selector('#Data Preparation')
                            .style({
                                'shape': 'round-tag',
                                'label': 'Data Preparation',
                                'background-color': '#61bffc',
                                'height': '65'
                            })
                            .update();
                    // Adjust the zoom level and fit settings
                    cy.ready(function () {
                        const container = document.getElementById('sigma-example');
                        const containerWidth = container.clientWidth;
                        const containerHeight = container.clientHeight;
                        const boundingBox = cy.elements().boundingBox();
                        const graphWidth = boundingBox.w;
                        const graphHeight = boundingBox.h;

                        const scaleX = containerWidth / graphWidth;
                        const scaleY = containerHeight / graphHeight;
                        const scale = Math.min(scaleX, scaleY);

                        //cy.zoom(scale);
                        cy.userZoomingEnabled(false);
                        cy.center();

                        // Adjust zoom level if needed to fit the content correctly
                        cy.fit(cy.elements(), 15); // Fit the graph with padding
                        //const rootNodes = cy.nodes().filter(node => node.incomers('edge').length === 0);
                        //rootNodes.forEach(rootNode => {
                        //rootNode.style('font-size', '35px');
                        //});
                        const analType = document.getElementById('formHidden1:analType').value;
                        const dataType = document.getElementById('formHidden1:dataType').value;

                    });
                    //reinitState();
                    setupButtons();
                });



    });
}

function setupTheme() {
    console.log("changecolors");
    changeAllNodesFontColor();

}


function setupButtons() {

    changeAllNodesFontColor();
    /*
     var element = $(parent.window.document).find("#menubuttonFormId\\:themelnk");
     element.bind('click keypress', function (event) {
     changeAllNodesFontColor();
     });
     */
    // Add interactivity (e.g., click event)
    /*
    cy.on('tap', 'node', function (evt) {
        var node = evt.target;
        const id = node.id();
        if (id === "Start") {
            const loggedIn = document.getElementById('formHidden1:loggedIn').value;
            if (loggedIn !== "false") {
                workflowRc();
            } else {
                PF("projectPreparedDialog").show();
            }
            return;
        } else if (id === "generateReport") {
            $('#formCircle\\:hiddenButton').click();
            return;
        }
        if (customMode) {
            if (["List", "Table", "Sanity Check", "Data processing"].indexOf(id) !== -1) {
                PF('growlWidget').show([{
                        severity: 'info',
                        summary: 'INFO',
                        detail: 'You can not unselect these steps, they are essential.'
                    }]);
            } else {
                if (node.data("highlight")) {
                    node.style({
                        'border-color': '#666', // Set the border color
                        'border-width': 2 // Set the border width
                    });
                    node.data("highlight", false);
                } else {
                    node.style({
                        'border-color': '#00dfff', // Set the border color
                        'border-width': 2 // Set the border width
                    });
                    node.data("highlight", true);

                }
            }
        } else {
            if (["List", "Table"].indexOf(id) !== -1) {

            } else {
                const domainUrl = document.getElementById('formHidden1:domainUrl').value;

                if (id === "Data Preparation") {
                    const naviType = document.getElementById('formHidden1:naviType').value;
                    if (["mset"].indexOf(naviType) !== -1) {
                        url = "/Secure/workflow/upload/ListUploadView.xhtml";
                        window.location = domainUrl + url;
                    } else if (["roc", "dose"].indexOf(naviType) !== -1) {
                        url = "/Secure/workflow/upload/UploadView.xhtml";
                        window.location = domainUrl + url;
                    } else if (naviType === "mf") {
                        PF("mfDialog").show();
                    } else if (naviType === "stat") {
                        PF("statDialog").show();
                    } else if (naviType === "enrich") {
                        PF("enrichDialog").show();
                    } else if (["path", "pathora", "pathqea"].indexOf(naviType) !== -1) {
                        PF("pathDialog").show();
                    } else if (["network", "dspc", "dspc-peak"].indexOf(naviType) !== -1) {
                        PF("networkDialog").show();
                    } else if (["mummichog"].indexOf(naviType) !== -1) {
                        PF("mumDialog").show();
                    } else if (["spec"].indexOf(naviType) !== -1) {
                        PF("specDialog").show();
                    }
                    return;
                }
                //console.log("running into line 324 url ==> " + url);
                //console.log("running into line 325 url ==> " + id);
                if (node.hasClass('highlighted-green') || node.hasClass('highlighted-red')) {

                    var url;
                    (async () => {
                        try {
                            url = await getUrl(id);
                            //console.log('URL received:', url);
                            if (url !== "" && url !== undefined) {
                                window.location = domainUrl + url;
                            }
                            //handleNodeClick([{name: 'clickedElement', value: id}]);
                        } catch (error) {
                            console.error('Error while getting URL:', error);
                        }
                    })();

                } else {
                    console.log("DONOT navigate after node click=====");

                    switch (id) {
                        case "Volcano":
                        case "PCA":
                        case "iPCA":
                        case "ANOVA":
                        case "Fold Change":
                        case "T-test":
                        case "Pattern Search":
                        case "Correlation Heatmap":
                        case "DSPC Networks":
                        case "PCA":
                        case "PLSDA":
                        case "sPLSDA":
                        case "OrthoPLSDA":
                        case "SAM":
                        case "EBAM":
                        case "Dendrogram":
                        case "Heatmap":
                        case "K-means":
                        case "SOM":
                        case "Random Forest":
                        case "SVM":
                        case "Network":
                        case "Name check":
                        case "Name check_List":
                        case "Name check_Table":
                        case "doMnetworkAnalysis_static":
                        case "doMnetworkAnalysis_metabo_phenotypes":
                        case "doMnetworkAnalysis_gene_metabolites":
                        case "doMnetworkAnalysis_metabo_metabolites":
                        case "doMnetworkAnalysis_global":
                        case "DSPC Network":
                        case "Network Viewer_dspc":
                        case "Univariate ROC":
                        case "Multivariate ROC":
                        case "Model-based ROC":
                        case "Metadata check":
                        case "Result":
                        case "metapaths_Method Selection":
                        case "metapaths Network Explorer pool":
                        case "Spectra Check":
                        case "Spectra View Result":
                            PF('growlWidget').show([{severity: 'info', summary: 'INFO', detail: 'Modifying parameters is not applicable to visual analytics methods.'}]);
                            return;
                            break;
                            ;

                        default:
                            console.log("Unknown analysis type");
                            break;
                    }

                    var naviType = document.getElementById('formHidden1:naviType').value
                    //console.log("running into line 382 url ==> " + naviType);
                   // console.log(naviType)
                    if (url !== "") {
                        var url;
                        (async () => {
                            try {
                                url = await getUrl(id);
                                navUrl = domainUrl + url;
                                setEditMode([]);

                            } catch (error) {
                                console.error('Error while getting URL:', error);
                            }
                        })();
                    }

                }
            }
        }

    });
    */
    cy.on('tap', 'edge', function (evt) {
        var edge = evt.target;
    });

    $('#formHelp\\:dldBn').bind('click keypress', function (event) {
        const networkState = cy.json();
        const networkStateJson = JSON.stringify(networkState);
        //console.log(networkStateJson)

        const blob = new Blob([networkStateJson], {type: "application/json"});
        const url = URL.createObjectURL(blob);
        const link = document.createElement("a");
        link.href = url;
        link.download = "network_state.json";
        link.click();
    });

    $('#formHelp\\:uploadBn').bind('click keypress', function (event) {
        PF("resDialog").show();
    });

    $('#formHelp\\:resetBn').bind('click keypress', function (event) {
        cy.nodes().forEach(function (n) {
            if (n.isParent() === false) { // Check if the node is not a container
                n.data('highlight', false);
                n.removeClass("highlighted-orange");
                n.removeClass("highlighted-green");
                n.removeClass("highlighted-red");

            }
        });
        cy.edges().forEach(function (e) {
            e.data('highlight', false);

            e.removeClass("highlighted-orange");
            e.removeClass("highlighted-green");
            e.removeClass("highlighted-red");
        });
        cy.layout({name: 'dagre'}).run();
    });

    $('#formHelp\\:editBn').bind('click keypress', function (event) {
        if (!customMode) {
            PF('growlWidget').show([{
                    severity: 'info',
                    summary: 'INFO',
                    detail: 'You can customize your workflow by clicking nodes in the network to unselect/select them. When you are ready, you can click on Start.'
                }]);
            cy.nodes().forEach(function (n) {
                if (n.isParent() === false) { // Check if the node is not a container
                    n.style({
                        'border-color': '#00dfff', // Set the border color
                        'border-width': 2 // Set the border width
                    });
                    n.data('highlight', true);
                }

            });
            customMode = true;
        } else {
            PF('growlWidget').show([{
                    severity: 'info',
                    summary: 'INFO',
                    detail: 'OK, custom selection mode is off!'
                }]);
            cy.nodes().forEach(function (n) {
                if (n.isParent() === false) { // Check if the node is not a container
                    n.style({
                        'border-color': 'gray', // Set the border color
                        'border-width': 2 // Set the border width
                    });
                    n.data('highlight', false);
                }
            });
            customMode = false;

        }
    });


}

function nodeSelect() {

    const analType = document.getElementById('formHidden1:analType').value;
    console.log(analType);
    //const inx = PF('treeWidget').selections;
    //const sel = PF('treeWidget').findNodes(inx)[0][0].outerText;
    //const inx = PF('treeWidget').selections;
    const sel = "All Analyses"
    showDescription("All Analyses");
    if (customMode) {
        predefinedWorkflow_custom();
    } else {
        if (analType === "mf") {
            predefinedWorkflow_multifac(sel);
        } else if (analType === "stat") {
            predefinedWorkflow(sel);
        } else if (analType === "roc") {
            predefinedWorkflow_biomarker(sel);
        } else if (["msetssp", "msetora", "msetqea"].indexOf(analType) !== -1) {
            predefinedWorkflow_enrichment(sel, analType);
        } else if (analType === "mummichog") {
            predefinedWorkflow_mummichog(sel);
        } else if (analType === "pathora") {
            predefinedWorkflow_pathora(sel);
        } else if (analType === "pathqea") {
            predefinedWorkflow_pathqea(sel);
        } else if (analType === "network") {
            predefinedWorkflow_network(sel);
        } else if (analType === "dose") {
            predefinedWorkflow_dose(sel);
        } else if (analType === "metadata") {
            predefinedWorkflow_metadata(sel);
        } else if (analType === "metapaths") {
            predefinedWorkflow_metapaths(sel);
        } else if (analType === "raw") {
            predefinedWorkflow_spectra(sel);
        }
    }

    selArray.unshift("Start");
    cy.nodes().forEach(function (n) {
        if (selArray.includes(n.id())) {
            n.style('border-color', '#00d2ff'); // Change node border color to blue for highlighting
            //n.style('background-opacity', 0.7);

            n.data('highlight', true);
        } else {
            n.style('border-color', '#222222'); // Change node border color to dark gray
            //n.style('background-opacity', 0.7);
            n.data('highlight', false);
        }
    });

    //let highlightedEdges = new Set(); // To track edges that should be highlighted
    /*
     cy.edges().forEach(function (e) {
     if (selArray.includes(e.source().id()) && selArray.includes(e.target().id())) {
     e.style('line-color', '#00d2ff'); // Change edge color to blue for highlighting
     highlightedEdges.add(e.id());
     } else {
     e.style('line-color', '#d6d6d6'); // Change edge color to light gray
     }
     });
     * */

}

function predefinedWorkflow_custom() {
    selArray = [];
    cy.nodes().forEach(function (n) {
        if (n.isParent() === false && n.data('highlight')) { // Check if the node is not a container
            selArray.push(n.id());
        }
    });
}

function predefinedWorkflow(sel) {

    const statNodes = [
        "Data Upload",
        "Sanity Check",
        "Filtering",
        "Normalization",
        "Visual Analytics"
    ];
    if (sel === "Differential Expression") {
        const array = ["Univariate Analysis",
            "Volcano",
            "Correlation Heatmap",
            "Classification and Feature Selection",
            "Random Forest",
            "SVM",
            "Pattern Search"];
        selArray = [...statNodes, ...array];
    } else if (sel === "Clustering analysis") {
        const array = ["Cluster Analysis",
            "K-means",
            "Heatmap",
            "Dendrogram",
            "Chemometrics Analysis",
            "PCA"];
        selArray = [...statNodes, ...array];
    } else if (sel === "Custom workflow") {
        clickMode = "selection";
        selArray = statNodes;
        PF('growlWidget').show([{severity: 'info', summary: 'INFO', detail: 'You can customize your workflow by selecting nodes in the network!'}]);
    } else if (sel === "All Analyses") {
        const array = ["Analysis Selection", "Univariate Analysis",
            "Volcano",
            "T-test",
            "Fold Change",
            "Correlation Heatmap",
            "Classification and Feature Selection",
            "Random Forest",
            "Cluster Analysis",
            "SVM",
            "SOM",
            "K-means",
            "Heatmap",
            "Dendrogram",
            "Chemometrics Analysis",
            "OrthoPLSDA",
            "sPLSDA",
            "PLSDA",
            "PCA",
            "Advanced Significance Analysis",
            "EBAM",
            "SAM",
            "Univariate Analysis",
            "Correlation Networks (DSPC)",
            "Pattern Search",
            "ANOVA"];
        selArray = [...statNodes, ...array];
    }
}


function predefinedWorkflow_biomarker(sel) {

    const statNodes = [
        "MetaboAnalyst",
        "Generic Format",
        "Biomarker Analysis",
        "Sanity Check",
        "Filtering",
        "Normalization"];
    if (sel === "Custom workflow") {
        clickMode = "selection";
        selArray = statNodes;
        PF('growlWidget').show([{severity: 'info', summary: 'INFO', detail: 'You can customize your workflow by selecting nodes in the network!'}]);
    } else if (sel === "All Analyses") {
        const array = [
            "Univariate ROC",
            "Multivariate ROC"
        ];
        selArray = [...statNodes, ...array];
    }
}

function predefinedWorkflow_multifac(sel) {

    const statNodes = [
        //"Multifactor Statistical Analysis",
        "Sanity Check",
        "Metadata Check",
        "Filtering",
        "Normalization",
        "Visual Analytics"
    ];

    const array = ["Supervise Classification",
        "Random Forest2",
        "Multivariate Analysis",
        "MEBA",
        "ASCA",
        "Univariate Analysis",
        "Multifactor anova",
        "Correlation Analysis",
        "Linear Models",
        "Data Overview",
        "Clustering heatmap",
        "iPCA",
        "Metadata Heatmap"
    ];
    selArray = [...statNodes, ...array];

}


function predefinedWorkflow_spectra(sel) {

    const statNodes = [
        "MetaboAnalyst"
    ];

    const array = [
        "Spectra Processing",
        "Spectra View Result",
        "Save Project"
    ];
    selArray = [...statNodes, ...array];

}

function predefinedWorkflow_enrichment(sel, analType) {
    var statNodes;
    if (analType === "msetqea") {
        statNodes = [
            "MetaboAnalyst",
            "Annotated Features",
            "Enrichment Analysis",
            "Table",
            "Annotation_Table",
            "Sanity Check_Table",
            "Filtering_Table",
            "Normalization_Table"
        ];
    } else if (analType === "msetssp") {
        statNodes = [
            "MetaboAnalyst",
            "Annotated Features",
            "Enrichment Analysis",
            "List With Concentration",
            "Annotation_Conc",
            "Comp. with Reference"

        ];
    } else {
        statNodes = [
            "MetaboAnalyst",
            "Annotated Features",
            "Enrichment Analysis",
            "List",
            "Annotation_List"
        ];
    }
    if (sel === "Custom workflow") {
        clickMode = "selection";
        selArray = statNodes;
        PF('growlWidget').show([{severity: 'info', summary: 'INFO', detail: 'You can customize your workflow by selecting nodes in the network!'}]);
    } else if (sel === "All Analyses") {
        var array;
        if (analType === "msetqea") {
            array = [
                "QEA"
            ];
        } else if (analType === "msetssp") {
            array = [
                "SSP"
            ];
        } else {
            array = [
                "ORA"
            ]
        }
        selArray = [...statNodes, ...array];
    }
}


function predefinedWorkflow_mummichog(sel) {
    const dataType = document.getElementById('formHidden1:dataType').value;
    //console.log(dataType);
    var statNodes;
    if (dataType === "mass_table") {
        statNodes = [
            "Table",
            "Sanity Check Intensity",
            "Filtering Intensity",
            "Normalization Intensity"
        ];
    } else {
        statNodes = [
            "List",
            "Sanity Check Peak"
        ];
    }
    if (sel === "Custom workflow") {
        clickMode = "selection";
        selArray = statNodes;
        PF('growlWidget').show([{severity: 'info', summary: 'INFO', detail: 'You can customize your workflow by selecting nodes in the network!'}]);
    } else {
        var array = [
            "Functional Annotation",
            "Scatter",
            "Network"
        ];
        if (dataType === "mass_table") {
            array.push("Heatmap");
        }
        selArray = [...statNodes, ...array];
    }
}

function predefinedWorkflow_pathqea(sel) {
    const dataType = document.getElementById('formHidden1:dataType').value;
    //console.log(dataType);
    var statNodes;
    statNodes = [
        "MetaboAnalyst",
        "Annotated Features",
        "Pathway Analysis",
        "Table",
        "Sanity Check_Table",
        "Annotation_Table",

        "Filtering",
        "Normalization"
    ];
    if (sel === "Custom workflow") {
        clickMode = "selection";
        selArray = statNodes;
        PF('growlWidget').show([{severity: 'info', summary: 'INFO', detail: 'You can customize your workflow by selecting nodes in the network!'}]);
    } else {
        var array = [
            "paBn_proceed_qea",
            "Results_Table"
        ];
        selArray = [...statNodes, ...array];
    }
    //console.log(selArray)
}


function predefinedWorkflow_pathora(sel) {
    const dataType = document.getElementById('formHidden1:dataType').value;
    //console.log(dataType);
    var statNodes;
    statNodes = [
        "MetaboAnalyst",
        "Annotated Features",
        "Pathway Analysis",
        "List",
        "Annotation_List"
    ];
    if (sel === "Custom workflow") {
        clickMode = "selection";
        selArray = statNodes;
        PF('growlWidget').show([{severity: 'info', summary: 'INFO', detail: 'You can customize your workflow by selecting nodes in the network!'}]);
    } else {
        var array = [
            "Name check_List",
            "paBn_proceed_ora",
            "Results_List"
        ];
        selArray = [...statNodes, ...array];
    }
    //console.log(selArray)
}


function predefinedWorkflow_network(sel) {
    const dataType = document.getElementById('formHidden1:dataType').value;
    var statNodes;
    //console.log(dataType);
    if (dataType === "conc") {
        statNodes = [
            "MetaboAnalyst",
            "Annotated Features",
            "Functional Analysis",
            "Network Analysis",
            "Table",
            "Sanity Check",
            "Annotation_Table",
            "Filtering",
            "Normalization",
            "Network Builder_dspc",
            "Network Viewer_dspc"
        ];
    } else {
        statNodes = [
            "MetaboAnalyst",
            "Annotated Features",
            "Network Analysis",
            "List",
            "Annotation_List_network",
            "Network Selection",
            "KEGG Network"
        ];
    }

    if (sel === "Custom workflow") {
        clickMode = "selection";
        selArray = statNodes;
        PF('growlWidget').show([{severity: 'info', summary: 'INFO', detail: 'You can customize your workflow by selecting nodes in the network!'}]);
    } else {
        var array = [

        ];
        selArray = [...statNodes, ...array];
    }
}

function predefinedWorkflow_metadata(sel) {

    var statNodes;
    statNodes = [
        "Data Processing",
        "Method Selection",
    ];
    if (sel === "Custom workflow") {
        clickMode = "selection";
        selArray = statNodes;
        PF('growlWidget').show([{severity: 'info', summary: 'INFO', detail: 'You can customize your workflow by selecting nodes in the network!'}]);
    } else {
        var array = [
            "Combine P-values",
            "Upset Diagram"
        ];
        selArray = [...statNodes, ...array];
    }
}


function predefinedWorkflow_dose(sel) {

    const statNodes = [
        "MetaboAnalyst",
        "Generic Format",
        "Dose Response Analysis",
        "Sanity Check",
        "Filtering",
        "Normalization",
        "DE Analysis"
    ]
    if (sel === "Custom workflow") {
        clickMode = "selection";
        selArray = statNodes;
        PF('growlWidget').show([{severity: 'info', summary: 'INFO', detail: 'You can customize your workflow by selecting nodes in the network!'}]);
    } else if (sel === "All Analyses") {
        const array = [
            "Curve Fitting"
        ];
        selArray = [...statNodes, ...array];
    }
}

function predefinedWorkflow_metapaths(sel) {

    const statNodes = [
        "Data Upload",
        "Data_Processing",
        "Method Selection",
        "Pathway-level integration"

    ]
    if (sel === "Custom workflow") {
        clickMode = "selection";
        selArray = statNodes;
        PF('growlWidget').show([{severity: 'info', summary: 'INFO', detail: 'You can customize your workflow by selecting nodes in the network!'}]);
    } else {
        const array = [
            "metapaths Network Explorer path",
            "metapaths Upset Diagram path"
        ];
        selArray = [...statNodes, ...array];
    }
}

var funcArrToStop = ["Annotation",
    "Sanity Check",
    "Filtering",
    "Normalization",
    "DE Analysis",
    "Network Builder_dspc"];

function handleRemoteCommandResponse(xhr, status, args) {
    if (args && args.textStatus) {
        console.log(args.jqXHR.pfArgs.res)
        return args.jqXHR.pfArgs.res;
    } else if (args && args.res) {
        return args.res;
    } else {
        console.error("No response received");
        return null;
    }
}

function workflowRc() {
    nodeSelect();
    let stopExecution = false;
    var lastFunc;
    const domainUrl = document.getElementById('formHidden1:domainUrl').value;
    $("#progressBar").show();
    // Function to execute a workflow step
    function executeFunction(func, index) {
        var url;
        (async () => {
            try {
                url = await getUrl(func);
                console.log('URL received:', url);
                // Proceed with your logic
            } catch (error) {
                console.error('Error while getting URL:', error);
            }
        })();
        return new Promise((resolve, reject) => {
            if (stopExecution) {
                return reject('Execution stopped');
            }
            setTimeout(() => {
                var promise = executeWorkflowRemoteCommand([{name: 'functionsStr', value: func}]);
                promise.then(function (responseData) {
                    const res = handleRemoteCommandResponse(null, null, responseData);
                    if (res === "ok" || res.includes("ok=")) {
                        cy.nodes().forEach(function (n) {
                            if (func === n.id()) {
                                n.removeClass("highlighted-orange");
                                n.addClass("highlighted-green");

                                //n.style('background-color', '#00FF00'); // Change node color to green for success
                                //n.style('background-opacity', 0.7);
                                n.data('highlight', true);
                            }
                        });

                        cy.edges().forEach(function (e) {
                            if (func === e.target().id()) {
                                if (e.source().hasClass('highlighted-green')) {
                                    e.removeClass("highlighted-orange");
                                    e.addClass("highlighted-green");

                                    //e.style('background-color', '#00FF00'); // Change node color to green for success
                                    //e.style('background-opacity', 0.7);
                                    e.data('highlight', true);
                                }
                            }
                        });
                        //console.log(func);
                        if (func === "Save Project") {
                            updateProcess();
                            PF('projRunDialog').show();
                        }
                        resolve();
                    } else if (res === "pending") {
                        cy.nodes().forEach(function (n) {
                            if (func === n.id()) {
                                n.removeClass("highlighted-orange");
                                n.addClass("highlighted-blue");
                                n.data('highlight', true);
                            }
                        });

                        cy.edges().forEach(function (e) {
                            if (func === e.target().id()) {
                                if (e.source().hasClass('highlighted-green')) {
                                    e.removeClass("highlighted-orange");
                                    e.addClass("highlighted-blue");
                                    e.data('highlight', true);
                                }
                            }
                        });
                        console.log(func);
                        if (func === "Save Project") {
                            updateProcess2();
                            PF('projSubmittedDialog').show();
                        }
                        resolve();
                    } else {
                        cy.nodes().forEach(function (n) {
                            if (func === n.id()) {
                                n.removeClass("highlighted-orange");
                                n.removeClass("highlighted-green");
                                n.addClass("highlighted-red");

                                n.style('background-color', '#FF0000'); // Change node color to red for error
                                n.style('background-opacity', 0.7);
                                n.data('highlight', false);
                            }
                        });

                        cy.edges().forEach(function (e) {
                            if (func === e.target().id()) {
                                if (e.source().hasClass('highlighted-green')) {

                                    e.removeClass("highlighted-orange");
                                    e.removeClass("highlighted-green");
                                    e.addClass("highlighted-red");

                                    e.style('background-color', '#FF0000'); // Change node color to red for error
                                    e.style('background-opacity', 0.7);
                                    e.data('highlight', false);
                                }
                            }
                        });
                        if (funcArrToStop.indexOf(func) !== -1) {
                            stopExecution = true;
                            reject('Error occurred');
                        } else {
                            resolve();
                        }
                    }


                    lastFunc = func; // Store the last function
                }).catch(function (error) {
                    stopExecution = true;
                    reject('Error occurred');

                    var sel = $("#currentSel");
                    sel.empty();
                    var description = "An error has occurred at step: ";

                    var link = $("<a>").attr("href", domainUrl + url).text("See more details:");
                    sel.append(description, link);
                });
            }, 500); // Add a delay of 0.5 seconds between each step
        });
    }

    // Ensure selArray is defined and available
    var analType = document.getElementById('formHidden1:analType').value;
    if (analType === "spec" || analType === "raw") {

    } else {
        selArray.unshift("Save Project");
    }
    //console.log("========== selArray======= ");
    //console.log(selArray);
    if (typeof selArray !== 'undefined' && selArray.length > 0) {
        //console.log("Starting execution of selArray:", selArray);
        // Create a sequence of promises to execute each function one by one
        selArray.reduce((promise, func, index) => {
            return promise.then(() => {
                //console.log("Executing function:", func);
                return executeFunction(func, index);
            });
        }, Promise.resolve())
                .then(() => {
                    if (!stopExecution) {
                        $("#progressBar").hide();

                        PF('growlWidget').show([{severity: 'info', summary: 'INFO', detail: 'Workflow has been executed successfully!'}]);
                        PF('statusDialog').hide();
                        var funcArrErr = document.getElementById('formHidden1:funcArrErr').value;
                        //console.log(funcArrErr);
                        var funcArrErr = [];
                        cy.nodes().forEach(function (n) {
                            if (n.hasClass("highlighted-red")) {
                                funcArrErr.push(n.data("label"));
                            }
                        });
                        var analType = document.getElementById('formHidden1:analType').value;
                        if (analType === "spec" || analType === "raw") {
                            return;
                        }

                        var textStr = "";
                        if (funcArrErr.length === 0 || funcArrErr[0] === "") {
                            textStr = '<span style="color: green;">All</span> functions have been performed successfully';
                        } else {
                            textStr = '<span style="color: red;">' + funcArrErr.length + '</span> functions have failed. For more details, please click on the red nodes in the workflow diagram.';
                        }

// Update the text content of the dialog
                        //console.log(document.getElementById('dialogText'))
                        //document.getElementById('dialogText').innerHTML = textStr;

// Show the dialog
                        startNavigationModule(textStr);


                    }
                })
                .catch(error => {
                    console.log('Workflow execution stopped:', error);
                });
    } else {
        console.error('selArray is not defined or empty');
    }
}

function enableReportWorkflowButton() {
    var reportWorkflowButton = document.getElementById('formCircle:reportWorkflow');

    // Check if the button exists
    if (reportWorkflowButton) {
        // Enable the button by removing the disabled attribute
        reportWorkflowButton.disabled = false;
        reportWorkflowButton.classList.remove('ui-state-disabled');
    } else {
        console.error("Button with ID 'reportWorkflow' not found.");
    }
}

function beforeSendHandler(args) {
    // Your beforeSend logic here
    const func = args[0].params[0].value; // Get the function string from the parameters

    cy.nodes().forEach(function (n) {
        if (n.id() === func) {
            n.addClass('highlighted-orange');
        }
    });

    cy.edges().forEach(function (e) {
        if (e.target().id() === func && e.source().hasClass('highlighted-green')) {
            e.addClass('highlighted-orange');
        }
    });

    function toggleFlash(cyElement) {
        cyElement.addClass('flashGreen');
    }
    cy.elements('.highlighted-orange').forEach(toggleFlash);
}

async function getUrl(func) {
    try {
        const result = await $.ajax({
            dataType: "html",
            type: "POST",
            url: '/MetaboAnalyst/faces/AjaxCall',
            data: {function: 'getWorkflowUrl', id: func},
            async: true, // Default, ensures non-blocking behavior
            cache: false,
        });
        return result;
    } catch (error) {
        console.error('AJAX error:', error);
        throw error; // Re-throw the error to be handled in the caller
    }
}



function getUrlOld(type) {
    let location = "";
    var analType = document.getElementById('formHidden1:analType').value;
    if (analType === "unknown") {
        analType = document.getElementById('formHidden1:naviType').value;
    }
    console.log("analType" + analType);
    console.log("Type" + type);
    if (analType === "stat") {
        switch (type) {
            case "Sanity Check":
                location = "/Secure/process/SanityCheck.xhtml";
                break;
            case "Filtering":
                location = "/Secure/process/FilterView.xhtml";
                break;
            case "Normalization":
                location = "/Secure/process/NormalizationView.xhtml";
                break;
            case "Visual Analytics":
                location = "/Secure/analysis/AnalysisView.xhtml";
                break;
            case "Correlation Heatmap":
                location = "/Secure/analysis/CorrelationView.xhtml";
                break;
            case "Random Forest":
                location = "/Secure/analysis/RFView.xhtml";
                break;
            case "Cluster Analysis":
                location = "/Secure/analysis/AnalysisView.xhtml";
                break;
            case "SVM":
                location = "/Secure/analysis/RSVMView.xhtml";
                break;
            case "SOM":
                location = "/Secure/analysis/SOMView.xhtml";
                break;
            case "K-means":
                location = "/Secure/analysis/KMView.xhtml";
                break;
            case "Heatmap":
                location = "/Secure/analysis/HeatmapView.xhtml";
                break;
            case "Dendrogram":
                location = "/Secure/analysis/TreeView.xhtml";
                break;
            case "OrthoPLSDA":
                location = "/Secure/analysis/OrthoPLSDAView.xhtml";
                break;
            case "sPLSDA":
                location = "/Secure/analysis/PLSDAView.xhtml";
                break;
            case "PCA":
                location = "/Secure/analysis/PCAView.xhtml";
                break;
            case "EBAM":
                location = "/Secure/analysis/EBAMView.xhtml";
                break;
            case "SAM":
                location = "/Secure/analysis/SAMView.xhtml";
                break;
            case "Correlation Networks (DSPC)":
                location = "/Secure/network/MphenoNetView.xhtml";
                break;
            case "Pattern Search":
                location = "/Secure/analysis/PatternView.xhtml";
                break;
            case "ANOVA":
                location = "/Secure/analysis/AnovaView.xhtml";
                break;
            case "Fold Change":
                location = "/Secure/analysis/FoldChangeView.xhtml";
                break;
            case "T-test":
                location = "/Secure/analysis/TtsetView.xhtml";
                break;
            case "Volcano":
                location = "/Secure/analysis/VolcanoView.xhtml";
                break;
            case "Visual Analytics":
            case "Advanced Significance":
            case "Chemometrics":
            case "Clustering":
            case "Classification":
            case "Univariate":
                location = "/Secure/analysis/AnalysisView.xhtml";
                break;
            default:
                location = "";
                break;
        }
    } else if (analType === "msetqea" || analType === "msetora" || analType === "enrich") {
        switch (type) {
            case "Annotation":
                location = "/Secure/process/NameCheckView.xhtml";
                break;
            case "Sanity Check":
                location = "/Secure/process/SanityCheck.xhtml";
                break;
            case "View Result":
                location = "/Secure/spectra/SpectraResult.xhtml";
                break;
            default:
                location = "";
                break;
        }
    } else if (analType === "mf") {
        switch (type) {
            case "Sanity Check":
                location = "/Secure/process/SanityCheck.xhtml";
                break;
            case "Filtering":
                location = "/Secure/process/FilterView.xhtml";
                break;
            case "Normalization":
                location = "/Secure/process/NormalizationView.xhtml";
                break;
            case "Visual Analytics":
                location = "/Secure/analysis/AnalysisView.xhtml";
                break;
            case "Random Forest2":
                location = "/Secure/multifac/MultifacRFView.xhtml";
                break;
            case "Multivariate Analysis", "Univariate Analysis":
                location = "/Secure/multifac/MultifacOverview.xhtml";
                break;
            case "MEBA":
                location = "/Secure/multifac/TimeCourseView.xhtml";
                break;
            case "Linear Models":
                location = "/Secure/multifac/LinearModelView.xhtml";
                break;
            case "ASCA":
                location = "/Secure/multifac/AscaView.xhtml";
                break;
            case "Multifactor anova":
                location = "/Secure/multifac/Anova2View.xhtml";
                break;
            case "Correlation Analysis":
                location = "/Secure/multifac/PartialCorrView.xhtml";
                break;
            case "Clustering heatmap":
                location = "/Secure/multifac/Heatmap2View.xhtml";
                break;
            case "ANOVA":
                location = "/Secure/multifac/Anova2View.xhtml";
                break;
            case "iPCA":
                location = "/Secure/multifac/LivePCAView.xhtml";
                break;
            case "Metadata Heatmap":
                location = "/Secure/multifac/MetaDataView.xhtml";
                break;
            default:
                location = "";
                break;
        }
    } else if (analType === "pathora" || analType === "pathqea") {
        switch (type) {
            case "Table":
            case "List":
                break;
            case "Sanity Check_Table":
                location = "/Secure/process/SanityCheck.xhtml";
                break;
            case "Annotation_List":
            case "Annotation_Table":
                location = "/Secure/process/NameMapView.xhtml";
                break;
            case "Filtering":
                location = "/Secure/process/FilterView.xhtml";
                break;
            case "Normalization":
                location = "/Secure/process/NormalizationView.xhtml";
                break;
            case "Results_Table":
            case "Results_List":
                const libOpt = document.getElementById('formHidden1:libOpt').value;
                if (libOpt.startsWith("smpdb")) {
                    location = "/Secure/pathway/SMPDBResultView.xhtml";
                } else {
                    location = "/Secure/pathway/PathResultView.xhtml";
                }
                break;
            case "paBn_proceed_qea", "paBn_proceed_ora":
                location = "/Secure/pathway/PathParamView.xhtml";
                break;
            default:
                location = "";
                break;
        }
    } else if (analType === "network") {

        switch (type) {
            case "Annotation":
                location = "/Secure/network/MnetMapView.xhtml";
                break;
            case "Network Selection":
                location = "/Secure/network/MnetParamView.xhtml";
                break;
            case "Network Builder_metabo_phenotypes":
            case "Network Builder_gene_metabolites":
            case "Network Builder_metabo_metabolites":
            case "Network Builder_global":
            case "Network Builder_dspc":
                var input = "Network Builder_metabo_phenotypes";
                var keyword = "Network Builder_";
                var result = input.substring(keyword.length);

                location = "/Secure/network/MnetStats.xhtml";
                break;
            case "Network Viewer_metabo_phenotypes":
            case "Network Viewer_gene_metabolites":
            case "Network Viewer_metabo_metabolites":
            case "Network Viewer_global":
            case "Network Viewer_dspc":
                var input2 = "Network Viewer_metabo_phenotypes";
                var keyword2 = "Network Viewer_";
                var result2 = input.substring(keyword.length);

                location = "/Secure/network/MphenoNetView.xhtml";
                break;
            case "KEGG Network":
                location = "/Secure/network/MetaboNetView.xhtml";
                break;
            case "paBn_proceed_qea", "paBn_proceed_ora":
                location = "/Secure/pathway/PathParamView.xhtml";
                break;
            default:
                location = "";
                break;
        }
    } else if (analType === "roc") {
        switch (type) {
            case "Sanity Check":
                location = "/Secure/process/SanityCheck.xhtml";
                break;
            case "Filtering":
                location = "/Secure/process/FilterView.xhtml";
                break;
            case "Normalization":
                location = "/Secure/process/NormalizationView.xhtml";
                break;
            case "Model-based ROC":
                location = "/Secure/roc/RocTestView.xhtml";
                break;
            case "Univariate ROC":
                location = "/Secure/roc/UnivRocView.xhtml";
                break;
            case "Multivariate ROC":
                location = "/Secure/roc/MultiRocView.xhtml";
                break;
            default:
                location = "";
                break;
        }
    } else if (analType === "metapaths") {
        switch (type) {
            case "Data_Processing":
                location = "/Secure/upload/MetaPathLoadView.xhtml";
                break;
            case "Method Selection":
                location = "/Secure/metapath/MetaPathAnalView.xhtml";
                break;
            case "Pooling peaks":
                location = "/Secure/mummichog/MummiResultView.xhtml";
                break;
            case "Pathway-level integration":
                location = "/Secure/metapath/MetaPathResultView.xhtml";
                break;
            case "metapaths Network Explorer path", "metapaths Network Explorer pool":
                location = "/Secure/network/MetaboNetView.xhtml";
                break;
            case "metapaths Upset Diagram path", "metapaths Upset Diagram pool":
                location = "/Secure/roc/RocTestView.xhtml";
                break;

            default:
                location = "";
                break;
        }
    } else if (analType === "mummichog") {
        switch (type) {
            case "Sanity Check Peak", "Sanity Check Intensity":
                location = "/Secure/process/SanityCheck.xhtml";
                break;
            case "Filtering Intensity":
                location = "/Secure/process/FilterView.xhtml";
                break;
            case "Normalization Intensity":
                location = "/Secure/process/NormalizationView.xhtml";
                break;
            case "Functional Annotation":
                location = "/Secure/mummichog/LibraryView.xhtml";
                break;
            case "Scatter":
                location = "/Secure/mummichog/MummiResultView.xhtml";
                break;
            case "Network":
                location = "/Secure/mummichog/KeggNetView.xhtml";
                break;
            case "Heatmap_mum":
                location = "/Secure/viewer/HeatmapView.xhtml";
                break;

            default:
                location = "";
                break;
        }
    } else if (analType === "metadata") {
        var statNodes;
        statNodes = [
            "Data Processing",
            "Method Selection"
        ];

        var array = [
            "Combine P-values",
            "Upset Diagram"
        ];
        selArray = [...statNodes, ...array];

        switch (type) {
            case "Data Processing":
                location = "/Secure/upload/MetaLoadView.xhtml";
                break;
            case "Method Selection":
                location = "/Secure/metastat/MetaAnalView.xhtml";
                break;
            case "Combine P-values", "Vote Counting", "Direct Merging":
                location = "/Secure/metastat/MetaResultView.xhtml";
                break;
            case "Upset Diagram":
                location = "/Secure/metastat/UpsetDiagramView.xhtml";
                break;
            default:
                location = "";
                break;
        }
    } else if (analType === "raw" || analType === "spec") {
        switch (type) {
            case "Spectra Processing":
                location = "/Secure/spectra/JobStatusView.xhtml";
                break;
            case "Parameters Setting":
                location = "/Secure/spectra/SpectraProcess.xhtml";
                break;
            case "View Result":
                location = "/Secure/spectra/SpectraResult.xhtml";
                break;
            default:
                location = "";
                break;
        }
    }

    return location;
}

function showDescription(type) {
    var stats = $("#currSelText");
    stats.empty();
    if (type === "Differential Expression") {
        stats.append("<p><strong>Differential Expression Analysis:</strong> This analysis identifies genes that are expressed at different levels between conditions or groups. It helps in understanding the biological differences between the states being compared.</p>");
    } else if (type === "Clustering analysis") {
        stats.append("<p><strong>Clustering Analysis:</strong> This analysis groups similar objects into clusters. It helps in identifying patterns or structures in the data, such as grouping genes with similar expression patterns.</p>");
    } else if (type === "Custom workflow") {
        stats.append("<p><strong>Custom Workflow:</strong> This allows you to define and run your own sequence of analyses tailored to your specific needs. You can combine different analysis steps by clicking on the node to select them and customize parameters to suit your research objectives.</p>");
    } else if (type === "All Analyses") {
        stats.append("<p><strong>All Analyses:</strong> This executes all analyses available to this module. Depending of the module, it might take a while to finish.</p>");
    } else {
        stats.append("<p>Select an analysis type to see the description here.</p>");
    }
}

function exportGraph() {
    // Get the current state of the graph, including node positions
    var jsonGraph = cy.json();

    // Convert the JSON object to a string
    var jsonString = JSON.stringify(jsonGraph, null, 2);

    // Create a blob from the JSON string
    var blob = new Blob([jsonString], {type: "application/json"});

    // Create a link element
    var link = document.createElement("a");

    // Set the download attribute with a filename
    link.download = "cytoscape_graph.json";

    // Create an object URL for the blob
    link.href = URL.createObjectURL(blob);

    // Append the link to the body
    document.body.appendChild(link);

    // Programmatically click the link to trigger the download
    link.click();

    // Remove the link from the document
    document.body.removeChild(link);
}

function reinitState() {
    var funcArr = document.getElementById('formHidden1:funcArr').value;
    var trimmedFuncArr = funcArr.slice(1, -1);
    // Split the string by commas and trim any extra whitespace from each element
    trimmedFuncArr.split(',').map(item => item.trim());
    //console.log(trimmedFuncArr);

    var funcArrErr = document.getElementById('formHidden1:funcArrErr').value;
    var trimmedFuncArrErr = funcArrErr.slice(1, -1);
    // Split the string by commas and trim any extra whitespace from each element
    trimmedFuncArrErr.split(',').map(item => item.trim());
    //console.log(trimmedFuncArrErr);

    cy.nodes().forEach(function (n) {
        if (trimmedFuncArr.indexOf(n.id()) !== -1) {
            n.addClass('highlighted-green');
        }
    });

    cy.edges().forEach(function (e) {
        if (trimmedFuncArr.indexOf(e.target().id()) !== -1) {
            if (e.source().hasClass('highlighted-green')) {
                e.addClass('highlighted-green');
            }
        }
    });

    cy.nodes().forEach(function (n) {
        if (trimmedFuncArrErr.indexOf(n.id()) !== -1) {
            n.addClass('highlighted-red');
        }
    });

    cy.edges().forEach(function (e) {
        if (trimmedFuncArrErr.indexOf(e.target().id()) !== -1) {
            e.addClass('highlighted-red');
        }
    });
}

function changeAllNodesFontColor() {
    var light = $(parent.window.document).find("#formHidden\\:selectedTheme").val();

    cy.nodes().forEach(function (node) {
        var textColor;
        if (light === "light") {
            textColor = '#222222';  // Light background -> black text, Dark background -> white text
        } else {
            textColor = '#ffffff';
        }
        node.style({
            'color': textColor // Adjust text color based on background brightness
        });
    });

    cy.edges().forEach(function (e) {
        var textColor;
        if (light === "light") {
            textColor = '#222222';  // Light background -> black text, Dark background -> white text
        } else {
            textColor = '#ffffff';
        }
        e.style({
            'line-color': textColor,
            'target-arrow-color': textColor
        });
    });
}
function startNavigationModule(textStr) {
    var analType = document.getElementById('formHidden1:analType').value;
    var pagesToVisit = [];
    if (analType === "stat") {
        if (selArray.indexOf("Heatmap") !== -1) {
            pagesToVisit.push('/MetaboAnalyst/Secure/analysis/HeatmapView.xhtml');
        }
    } else if (analType === "mf") {
        if (selArray.indexOf("Clustering heatmap") !== -1) {
            pagesToVisit.push('/MetaboAnalyst/Secure/multifac/Heatmap2View.xhtml');
        }
    }
    console.log(pagesToVisit)
    localStorage.setItem('pagesToVisit', JSON.stringify(pagesToVisit));

    // Navigate to the first page after a short delay
    if (pagesToVisit.length > 0) {
        setTimeout(function () {
            setSummaryText([{name: 'summaryText', value: textStr}]);
            //document.getElementById('dialogText').innerHTML = "";
            window.location.href = pagesToVisit[0];
        }, 2000); // Delay before navigating to the first page
    } else {
        setSummaryText([{name: 'summaryText', value: textStr}]);

        //document.getElementById('dialogText').innerHTML = textStr;
        PF("resDialog").show();
    }
}


function setEditModeCallBack() {
    const naviType = document.getElementById('formHidden1:naviType').value;
    window.location = navUrl;
}

function calculateLongestPath(cy) {
    let maxDepth = 0;

    // Find the first edge and its source node
    const firstEdge = cy.edges()[0];
    if (!firstEdge) {
        console.warn('No edges found in the graph.');
        return 0; // Return 0 if no edges exist
    }

    const rootNode = firstEdge["_private"]["data"]["source"];
    console.log(rootNode)

    if (!rootNode) {
        console.warn('Source node of the first edge is invalid.');
        return 0; // Return 0 if the source node is invalid
    }

    // Perform BFS starting from the root node
    cy.elements().bfs({
        roots: "#" + rootNode,
        visit: (v, e, u, i, depth) => {
            console.log(`Visiting node: ${v.id()}, Depth: ${depth}`);
            console.log(depth);
            maxDepth = Math.max(maxDepth, depth); // Track the maximum depth
        },
        directed: true // Respect directed edges
    });

    console.log('Calculated Longest Path (Max Depth):', maxDepth);
    return maxDepth;
}


function startNavigation(pagesToVisit) {
    //console.log("pages=============");
    localStorage.setItem('pagesToVisit', JSON.stringify(pagesToVisit));
    // Navigate to the first page after a short delay
    //console.log("pagesToVisit = " + pagesToVisit)
    setTimeout(function () {
        window.location.href = pagesToVisit[0];
    }, 2000); // Delay before navigating to the first page
}
