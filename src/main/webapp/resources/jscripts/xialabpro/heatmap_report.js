function initReportFunctions() {

    $('#reportBn').bind('click keypress', function (event) {
        handleEvent(false);
    });

    var element = $(parent.window.document).find("#sidebar-form\\:m_report");

    element.bind('click keypress', function (event) {
        handleEvent(true);
    });

}

function handleEvent(sendJsonAsync) {
    const main_map = heat.main_map();
    var focusNms = main_map.nms;
    savedState.focusNames = focusNms;
    savedState.resolution = parseInt($('#resolution').val());
    savedState.border = $('#border').val();
    savedState.colorSchema = $('#color').val();
    savedState.peak_cluster = $('#peak-cluster').val();
    if (localStorage.getItem('pagesToVisit')) {
        var pagesToVisit = JSON.parse(localStorage.getItem('pagesToVisit'));
        pagesToVisit.shift();
        localStorage.setItem('pagesToVisit', JSON.stringify(pagesToVisit));
        setTimeout(function () {

                sendImageToServer(heat.download_image("heatmap"), "heatmap_" + heatmapType, "png", function () {
                    if (pagesToVisit.length > 0) {
                        window.parent.location.href = pagesToVisit[0];
                    } else {
                        localStorage.setItem('reportEndBool', true);
                        window.parent.location.href = "/MetaboAnalyst/Secure/xialabpro/WorkflowView.xhtml";
                    }
                });

        }, 3000);

    } else {
        sendImageToServer(heat.download_image("heatmap"), "heatmap_" + heatmapType, "png");
        sendJsonToServer(JSON.stringify(savedState), "heatmap_" + heatmapType, sendJsonAsync);
    }
}

function checkSavedState(heatmapName) {

    fetch(usr_dir + '/report_' + heatmapName)
            .then(response => {
                if (response.ok) {
                    console.log('Saved state exists');
                    $.getJSON(usr_dir + '/report_' + heatmapName, function (data) {
                        savedState = data;
                        reloadSavedState(savedState);
                    });
                } else {
                    console.log("Saved state doesn't exist");

                    parent.PF("statusDialog").show();
                    setTimeout(function () {
                        handleEvent(false);
                        parent.PF("statusDialog").hide();
                    }, 2500);
                    //Execute if file doesn't exist
                }
            })
            .catch(error => {
                // Handle 404 errors here
                if (error && error.response && error.response.status === 404) {
                    console.log('File not found:', error.message);
                } else {
                    // Handle other network errors here if needed
                    console.log('There was a problem with the fetch operation:', error.message);
                }

            });

}


function reloadSavedState(savedState) {
    console.log(savedState)
    if (savedState.resolution !== $('#resolution').val()) {
        $('#resolution').val(savedState.resolution);
        resolution = savedState.resolution;
    }
    if (savedState.border !== $('#border').val()) {
        $('#border').val(savedState.border);
        border = savedState.border;
    }


    if (savedState["colorSchema"] !== $('#color').val()) {
        $('#color').val(savedState.colorSchema);
        heat.updateHeatmapColors(savedState.colorSchema);
    }

    if (savedState.peak_cluster !== $('#peak-cluster').val()) {
        $('#peak-cluster').val(savedState.peak_cluster);
        heat.clustergenes($('#peak-cluster').val());
    }

    if ("enrichFileName" in savedState) {
        $("#enrichdb").val(savedState.enrichdb);
        reloadEnrichmentTest(savedState.enrichFileName);
    }

    if ("focusNames" in savedState) {
        heat.setup_focus_view(savedState.focusNames);
    }

    heat.draw();


}

function reloadEnrichmentTest(fileName) {

    if (heatmapType === "mummichog") {
        $.getJSON(usr_dir + fileName, function (raw_data) {
            clearAnnot()

            //var query = $('#queryView').val();
            var query = "focus";
            peakTable = raw_data["peakTable"]
            var keggArr = Array(peakTable["Matched.Compound"].length).fill(0);
            ;
            for (var i = 0; i < peakTable["Matched.Compound"].length; i++) {
                var kegg = peakTable["Matched.Compound"][i]
                var inx = keggConv["kegg"].indexOf(kegg)
                if (inx !== -1) {
                    keggArr[i] = keggConv["compound"][inx]
                }
            }
            //console.log(peakTable["Matched.Compound"])
            //console.log(keggArr)
            peakTable["Compound.Name"] = keggArr
            if (query === "focus") {
                fun_pval = raw_data['fisher.p'];
                fun_padj = raw_data['fisher.p'];
            } else {

                fun_pval = raw_data['comb.p'];
                fun_padj = raw_data['comb.p'];
            }
            global_fun_anot = raw_data['path.nms'];
            hit_num = raw_data['hits.all'];
            hit_sig = raw_data['hits.sig'];

            //console.log(hit_sig)
            set_link = raw_data['fun.link'];
            fun_ids = raw_data['fun.ids'];
            var data_grid = $('#dg2');

            //empty if there is any
            data_grid.datagrid('loadData', {
                "total": 0,
                "rows": []
            });
            var cur_inx = 0;
            var idx = 0
            var arr = []
            _(global_fun_anot).each(function (v, k) {
                arr.push({
                    pathname: global_fun_anot[idx],
                    hit: hit_num[idx].length,
                    sig: hit_sig[idx].length,
                    pval: fun_pval[idx],
                    color: '<div id=\"function_' + idx + '\" onclick="openModuleColorPicker(this);event.stopPropagation()" style="width:30px:height:20px;text-align:center"></div>'
                })
                idx++;
                cur_inx = cur_inx + 1;
            });
            $('#dg2').datagrid('loadData', arr);
            if (query === "focus") {
                names_focus_reset = names_focus
            }
        });
    } else {
        $.getJSON(usr_dir + '/' + result, function (raw_data) {
            global_fun_anot = raw_data['fun.anot'];

            fun_pval = raw_data['fun.pval'];
            fun_padj = raw_data['fun.padj'];
            hit_num = raw_data['hit.num'];
            total = raw_data['total'];
            set_link = raw_data['fun.link'];
            fun_ids = raw_data['fun.ids'];
            var data_grid = $('#dg2');
            //empty if there is any
            data_grid.datagrid('loadData', {
                "total": 0,
                "rows": []
            });
            var cur_inx = 0;
            _(global_fun_anot).each(function (v, k) {
                data_grid.datagrid('appendRow', {
                    pathname: k,
                    pval: fun_pval[cur_inx],
                    color: '<div id=\"function_' + cur_inx + '\" onclick="openModuleColorPicker(this);event.stopPropagation()" style="width:30px:height:20px;text-align:center"></div>',
                    hit: hit_num[cur_inx] //+ "/" + total[cur_inx]
                });
                cur_inx = cur_inx + 1;
            });
        });
    }
    names_focus_reset = names_focus

}