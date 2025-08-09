var isLts = false;
var savedState = {};
var path_nm_padding = 0;
var current_enr_names = []
var path_names = [];
var annot_colors = []
var annot_arr = [];
var annot_arr2 = [];//annotation
var annot_arr3 = [];//metabolite name
var annot_inx_arr = [];
var annot_arr_length = [0]
var annot_inx = 0;
var sel_inx = [];
var unselType = "default"
var takingImage = false;
var mainMapWidth = 0;
var currColor = '#e6194b';
var twentyColors = ['#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', '#46f0f0', '#f032e6', '#bcf60c', '#fabebe', '#008080', '#e6beff', '#9a6324', '#fffac8', '#800000', '#aaffc3', '#808000', '#ffd8b1', '#000075', '#808080', '#ffffff', '#000000']
var current_module_color = '#e6194b';
var main_map, smpl_map;
var init_focus_genes = 50;
var libOpt = "";
var containsTstat = false;
var enrich_name;
var textColor = 'rgba(255, 255, 255, 1)';
var heat = {};
var bgColor = 'rgba(255, 255, 255, 1)'; //for png save

function initHeatMap() {

    if ($(parent.window.document).find("#sidebar-form\\:m_report")) {
        isLts = true;
    }

    names_focus = [];
    path_focus = 0;
    path_focus_names = [];
    cluster_methods = ["ward", "average", "single", "complete"];

    var url = '/MetaboAnalyst/faces/AjaxCall?function=getheatmap' + "&ignoreMe=" + new Date().getTime()

    var res = $.ajax({
        type: "GET",
        url: url,
        async: false
    }).responseText;
    var info = res.split("||");
    usr_dir = info[0];
    org = info[1];
    heatmapType = info[3];
    if (heatmapType !== "mummichog") {
        libOpt = info[4];
    }
    //console.log(libOpt)
    heat = initHeatMapFunctions();
    all_in_focus = 0;
    var heatmapName = info[2];

    if (heatmapType === "pathway") {
        $('#dg2').datagrid({
            columns: [[
                    {field: 'pathname', title: 'Name', width: 160},
                    {field: 'hit', title: 'Hits', width: 40},
                    {field: 'pval', title: 'P-val', width: 60},
                    {field: 'color', title: 'Color', width: 40}
                ]]
        });
        //$("#peakViewOpt").hide();
    }

    $.getJSON(usr_dir + '/' + heatmapName, function (raw_data) {
        //org = raw_data["org"];
        if (heatmapType !== "mummichog") {
            org = raw_data["org"];
        }
        //console.log(raw_data)
        //format data object due to switch from rjsonio to rjson

        var numberOfSamples = raw_data["sample.names"].length
        var newData = [];
        var sub_array = []
        for (var i = 0; i < raw_data["data"].length; i++) {

            sub_array.push(raw_data['data'][i])

            if (sub_array.length === numberOfSamples) {
                newData.push(sub_array)
                sub_array = []

            }
        }

        raw_data['data'] = newData;


        setEnrichOpts(org);
        //console.log(raw_data)
        //raw data
        global_gene_anot = {};

        data_type = raw_data['data.type'];
        var gene_id = raw_data['gene.id'];
        var gene_entrez = raw_data['gene.entrez'];
        var gene_nm = raw_data['gene.name'];

        set_link = raw_data['set.link'];
        global_fun_anot = raw_data['fun.anot'];
        fun_ids = raw_data['fun.ids'];
        fun_pval = raw_data['fun.pval'];
        fun_padj = raw_data['fun.padj'];
        total = raw_data['total'];
        hit_num = raw_data['hit.num'];
        var gene_cluster = raw_data['gene.cluster'];
        var sample_cluster = raw_data['sample.cluster'];
        sample_names = raw_data['sample.names'];
        var meta_data = raw_data['meta'];
        meta_anot = raw_data['meta.anot'];
        var expr_data = raw_data['data'];
        //console.log(expr_data)
        //set up clustering data (gene & sample)
        var gene_cluster_array = [];
        var gene_cluster_names = []; //names of data row

        _(gene_cluster).each(function (v, k) { //k is gene name, v is expression vector 
            gene_cluster_names.push(k);
            for (var i = 0; i < v.length; i++) {
                v[i] = v[i] + 7
            }
            for (var i = 0; i < 7; i++) {
                v.unshift(i)
                v.push(Math.max(v) + i)
            }

            gene_cluster_array.push(v);

        });
        var pval_arry = gene_cluster['pval'];
        var cluster_types = Object.keys(gene_cluster)
        if (cluster_types.indexOf("stat") !== -1) {
            var tstat_arry = gene_cluster['stat'];
            containsTstat = true;
        }


        heat.gene_cluster(gene_cluster_array);
        heat.gene_cluster_names(gene_cluster_names);

        var sample_cluster_array = [];
        var sample_cluster_names = []; //names of data row
        _(sample_cluster).each(function (v, k) { //k is gene name, v is expression vector 
            sample_cluster_names.push(k);
            sample_cluster_array.push(v);
        });
        heat.sample_cluster(sample_cluster_array);
        heat.sample_cluster_names(sample_cluster_names);

        //set up expression data (each element is a gene expression vector)
        var expr_array = []; //each element is expression vector 
        var names = []; //names of data row
        //pval_arry = pval_arry.splice(7)
        _(expr_data).each(function (v, k) { //k is index, v is expression vector 
            var id = gene_id[k];
            names.push(id);
            expr_array.push(v);
            if (gene_cluster === 1) {
                global_gene_anot[id] = [gene_entrez[k], gene_nm[k]];
            } else {
                if (containsTstat) {
                    global_gene_anot[id] = [gene_entrez[k], gene_nm[k], pval_arry[k + 7], tstat_arry[k + 7]];
                } else {
                    global_gene_anot[id] = [gene_entrez[k], gene_nm[k], pval_arry[k + 7]]
                }
            }

        });
        var zeroArr = [];
        for (var i = 0; i < expr_array[0].length; i++) {
            zeroArr[i] = "hidden";
        }

        heat.data(expr_array, 1);
        heat.names(names, 1);

        padded_data = expr_array;
        padded_names = names;
        for (var i = 0; i < 7; i++) {
            padded_data.unshift(zeroArr)
            padded_data.push(zeroArr)
            padded_names.push("und");
            padded_names.unshift("und");
        }

        //set up meta data and sort options
        lbl_array = []; //each element is sample labels
        phenos = []; //contain names of meta_info row
        //var sel = $("#smpl-cluster");
        _(meta_data).each(function (v, k) {
            //    sel.append('<option value="' + k + '">' + k + '</option>');
            phenos.push(k);
            lbl_array.push(v);
        });

        heat.meta(lbl_array);
        heat.meta_names(phenos);
        heat.meta_anot(meta_anot);
        //defaults
        $('#heatmap-wrap').hide();
        $('#meta-wrap').hide();

        //first sort based on p values
        //heat.clustergenes('pval');
        heat.draw();
        //heat.clustergenes('pval');
        //heat.draw();

        //load the datagrid
        loadDataGrid();
        //loadSmplDataGrid();
        setHeatmapEnrichOpts(org, heatmapType);

        if (isLts) {
            checkSavedState(heatmapName);
        }

        setupTheme();
        $("#spinner").fadeOut("slow");
        if (heatmapType === "mummichog") {
            $("#initdialog").dialog('open');
        }
        if (isLts) {
            initReportFunctions();
        }
    });
}
;

/* utility functions */
// invert mapping to get indices
//need to adjust for gutsize for translate
function indices(totwidth, totheight, gutsize, e) {
    var x = e.pageX - $(e.target).offset().left - gutsize;
    var y = e.pageY - $(e.target).offset().top - gutsize;
    var j = Math.floor(x / totwidth);
    var i = Math.floor(y / totheight);
    return {
        x: x,
        y: y,
        i: i,
        j: j
    };
}
;

// lookup value with indices
function lookup(pos, data) {
    if (pos.i < 0 || pos.i > data.length) { //y
        return undefined;
    }
    if (pos.j < 0 || pos.j > data[0].length) { //y
        return undefined;
    }
    return data[pos.i][pos.j]; // return the value of the clicked spot
}
;


//sort a 2d array based on a give names
//similar to sort a table by column names
function sort2darrayByCol(names, data) {
    //first transpose to column as element
    var data_transpose = Object.keys(data[0]).map(function (c) {
        return data.map(function (r) {
            return r[c];
        });
    });

    //combine names and columns into object
    //add original index to make sure stable sort (when equal)
    var data_combine = [];
    for (var i = 0, l = names.length; i < l; i++) {
        var obj = {
            name: names[i],
            index: i,
            value: data_transpose[i]
        };
        data_combine.push(obj);
    }

    //sort based on name
    data_combine.sort(function (x, y) {
        if (x.name === y.name) {
            return x.index - y.index;
        }
        return x.name - y.name;
    });

    //extract sorted array
    var expr_arry = [];
    for (var i = 0; i < data_combine.length; i++) {
        expr_arry.push(data_combine[i].value);
    }

    //tranpose back
    var arry_sorted = Object.keys(expr_arry[0]).map(function (c) {
        return expr_arry.map(function (r) {
            return r[c];
        });
    });
    return arry_sorted;
}

//sort array by row index, need to syncro same time
function sort2darrayByRow(clustinx, data, clustdata, genenames) {
    //first transpose to column as element
    var data_trans = Object.keys(data[0]).map(function (c) {
        return data.map(function (r) {
            return r[c];
        });
    });

    var all_data_trans = clustdata.concat(data_trans, genenames);

    //back to row element
    var all_data = Object.keys(all_data_trans[0]).map(function (c) {
        return all_data_trans.map(function (r) {
            return r[c];
        });
    });

    all_data.sort(function (x, y) {
        return x[clustinx] - y[clustinx];
    });

    //tranpose back to column as element
    all_data_trans = Object.keys(all_data[0]).map(function (c) {
        return all_data.map(function (r) {
            return r[c];
        });
    });

    //extract sorted array, both clustdata and names are column
    var sorted_clustdata = all_data_trans.slice(0, clustdata.length);
    var sorted_names = all_data_trans.slice(all_data_trans.length - 1, all_data_trans.length);

    //need to transform, as orginal is row as column
    var expr_arry_trans = all_data_trans.slice(clustdata.length, all_data_trans.length - 1);
    var expr_arry = Object.keys(expr_arry_trans[0]).map(function (c) {
        return expr_arry_trans.map(function (r) {
            return r[c];
        });
    });

    return [expr_arry, sorted_clustdata, sorted_names];
}

function isCanvasSupported() {
    var elem = document.createElement('canvas');
    return !!(elem.getContext && elem.getContext('2d'));
}

function loadDataGrid() {
    var data_grid = $('#dg');
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
            padj: fun_padj[cur_inx],
            hits: hit_num[cur_inx] + "/" + total[cur_inx]
        });
        cur_inx = cur_inx + 1;
    });
    data_grid.datagrid({
        onLoadSuccess: function (data) {
            $('#dg').datagrid('getPanel').find('div.datagrid-header input[type=checkbox]').attr('disabled', 'disabled');
        }
    });
}

function doSearch() {
    var term = $('#itemid').val();
    var data_grid = $('#dg');
    data_grid.datagrid('loadData', {
        "total": 0,
        "rows": []
    });
    var cur_inx = 0;
    _(global_fun_anot).each(function (v, k) {
        if (k.toLowerCase().indexOf(term.toLowerCase()) !== -1) {
            data_grid.datagrid('appendRow', {
                pathname: k,
                hits: hit_num[cur_inx] + "/" + total[cur_inx],
                pval: fun_pval[cur_inx],
                padj: fun_padj[cur_inx]
            });
        }
        cur_inx = cur_inx + 1;
    });
}

function reloadFull() {
    loadDataGrid();
}

function loadSmplDataGrid() {
    var data_grid = $('#dg2');
    //create columns dynamically
    var menuStruct = [];
    var menuItems = [];
    var smpl_col = {
        field: 'Sample',
        title: 'Sample',
        width: 80
    };
    menuItems.push(smpl_col);
    _(phenos).each(function (v, k) {
        var meta_col = {
            field: v,
            title: v,
            width: 80
        };
        menuItems.push(meta_col);
    });

    menuStruct.push(menuItems);
    data_grid.datagrid({
        columns: menuStruct
    });
    //add content
    _(sample_names).each(function (v, k) {
        var my_row = {};
        my_row['Sample'] = v;
        _(phenos).each(function (v2, k2) {
            my_row[v2] = meta_anot[lbl_array[k2][k]];
        });
        data_grid.datagrid('appendRow', my_row);
    });
}

function doFocusEnrichmentTest() {
    testFocusEnrichment(function (result) {
        if (result.startsWith('ERROR!')) {
            var errMsg = result.substring(6);
            $.messager.alert('Error', 'Failed to process the request!' + errMsg, 'error');
        } else {
            $.getJSON(usr_dir + '/' + result, function (raw_data) {
                focus_fun_anot = raw_data['fun.anot'];
                fun_ids = raw_data['fun.ids'];
                var focus_pval = raw_data['fun.pval'];
                var focus_hit_num = raw_data['hit.num'];

                var data_grid = $('#dg1');
                //empty if there is any
                data_grid.datagrid('loadData', {
                    "total": 0,
                    "rows": []
                });
                var cur_inx = 0;
                _(focus_fun_anot).each(function (v, k) {
                    data_grid.datagrid('appendRow', {
                        pathname: k,
                        pval: focus_pval[cur_inx],
                        hits: focus_hit_num[cur_inx]
                    });
                    cur_inx = cur_inx + 1;
                });
            });
        }
        $.messager.progress('close');
    });
}

function exportResultTable() {
    if ($('#dg2').datagrid('getRows').length === 0) {
        $.messager.alert('Error', 'No functional enrichment analysis has been performed!', 'error');
        return;
    }
    setupFileDownload(enrich_name + ".csv");
}

function setupFileDownload(result) {
    var fileLnk = $("#fileLnk");
    fileLnk.empty();
    fileLnk.append("Right click the link below, then 'Save Link As ... ' to download the file<br/><br/>");
    fileLnk.append('<strong><a href="' + usr_dir + '/' + result + '" target="_blank"><u>' + result + '</u></a></strong>');
    $.messager.progress('close');
    $("#filedialog").dialog('open');
}

function getCurrentErrorMsg() {
    var errorMsg = $.ajax({
        type: "GET",
        url: '/MetaboAnalyst/faces/AjaxCall?function=getErrorMsg' + "&ignoreMe=" + new Date().getTime(),
        async: false
    }).responseText;
    return errorMsg;
}

function doEnrichmentTest() {
    var type = heatmapType;
    testEnrichment(type, function (result) {
        savedState.enrichFileName = result;
        savedState.enrichdb = $("#enrichdb").val();
        enrich_name = result.substring(0, result.length - 5);

        if (result.startsWith('ERROR!')) {
            var errMsg = result.substring(6);
            $.messager.alert('Error', 'Failed to process the request!' + errMsg, 'error');
        } else {
            if (heatmapType === "mummichog") {
                $.getJSON(usr_dir + result, function (raw_data) {
                    //console.log(raw_data)
                    clearAnnot();

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
                    hit_num = raw_data['hits.all']; // should be array
                    if (!Array.isArray(hit_num)) {
                        hit_num = Object.values(hit_num);
                    }
                    hit_sig = raw_data['hits.sig'];
                    if (!Array.isArray(hit_sig)) {
                        hit_sig = Object.values(hit_sig);
                    }
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

        $.messager.progress('close');

    });
}

function testEnrichment(type, callBack) {
    var fundb = $('#enrichdb').val();

    if (org === "cjo") {
        if (fundb !== "kegg") {
            $.messager.alert('',
                    "Only KEGG information is available for this organism",
                    'error');
            return;
        }
    }
    ;

    var query = $('#queryView').val();

    var names_arr = [];
    if (names_focus.length === 0) {
        $.messager.alert('',
                "No features found in the Focus View. Please use your mouse to select a pattern from Overview.",
                'error');
        return("ERROR!");
    }
    var entrez_ids = global_gene_anot[names_focus[0]][0];
    for (var i = 1, l = names_focus.length; i < l; i++) {
        entrez_ids = entrez_ids + '; ' + global_gene_anot[names_focus[i]][0];
        names_arr.push(names_focus[i])
    }

    current_enr_names = names_arr;
    if (query === "overall") {
        entrez_ids = "overall";
    }
    $('#dg').datagrid('loadData', {
        "total": 0,
        "rows": []
    });

    var data;
    //console.log(type)
    if (type === "mummichog") {
        fundb = org + "_" + fundb;
        data = {function: 'heatmapMummichog', funDB: fundb, IDs: entrez_ids, type: "focus"};
    } else {
        if (libOpt.includes("smpdb")) {
            fundb = "smpdb";
        } else {
            fundb = "keggc";
        }
        data = {function: 'heatmapPathway', funDB: fundb, IDs: entrez_ids, type: "focus"};
    }

    $.ajax({
        beforeSend: function () {
            $.messager.progress({
                text: 'Processing .....'
            });
        },
        dataType: "html",
        type: "POST",
        url: '/MetaboAnalyst/faces/AjaxCall',
        data: data,
        async: true,
        cache: false,
        success: function (result) {
            return callBack(result);
        },
        error: function () {
            $.messager.alert('Error', "Failed to process the request!");
            $.messager.progress('close');
        }
    });
}

function doGlobalTest() {
    testGlobalEnrichment(function (result) {
        if (result.startsWith('ERROR!')) {
            var errMsg = result.substring(6);
            $.messager.alert('Error', 'Failed to process the request!' + errMsg, 'error');
        } else {
            $.getJSON(usr_dir + '/' + result, function (raw_data) {
                var data_name = raw_data['data.nm'];
                var fun_pval = raw_data['raw.pval'];
                var hit_num = raw_data['hit.num'];

                var data_grid = $('#dg1');
                //empty if there is any
                data_grid.datagrid('loadData', {
                    "total": 0,
                    "rows": []
                });
                var cur_inx = 0;
                _(data_name).each(function (v, k) {
                    data_grid.datagrid('appendRow', {
                        dataname: data_name[k],
                        hit: hit_num[cur_inx],
                        pval: fun_pval[cur_inx]
                    });
                    cur_inx = cur_inx + 1;
                });
            });
        }
        $.messager.progress('close');
    });
}

function testGlobalEnrichment(callBack) {

    var ids = $('#signature1').val().split('\n');
    var entrez_ids = ids[0];
    for (var i = 1, l = ids.length; i < l; i++) {
        entrez_ids = entrez_ids + '; ' + ids[i];
    }

    $('#dg1').datagrid('loadData', {
        "total": 0,
        "rows": []
    });
    $.ajax({
        beforeSend: function () {
            $.messager.progress({
                text: 'Processing .....'
            });
        },
        dataType: "html",
        type: "POST",
        url: '/MetaboAnalyst/faces/AjaxCall',
        data: {function: 'doGlobalEnrichmentTest', entrezIDs: entrez_ids},
        async: true,
        cache: false,
        success: function (result) {
            return callBack(result);
        },
        error: function () {
            $.messager.alert('Error',"Failed to process the request!");
            $.messager.progress('close');
        }
    });
}


function initHeatMapFunctions() {

    var data = [];
    var data_focus = [];
    var selected_data = [];
    var meta_data = [];
    var meta_anot = {}; //annotation for meta (number to string)
    var gene_cluster = [];
    var gene_cluster_focus = [];
    var gene_cluster_names = [];
    var sample_cluster = [];
    var sample_cluster_names = [];
    var names = [];//gene names

    var selected_names = [];
    var meta_names = [];
    var timer; //use a timer to distinguish single and double click

    //heatmap object (total 7) editor map is a dialog for column/sample editing

    var overmeta_map, over_map, selected_map, meta_map;

    jQuery("#custom").spectrum({
        color: "#fff",
        flat: true,
        showInitial: true,
        cancelText: "", // not shown, as this is not working in flat mode        
        change: function (color) {
            var col = color.toHexString()
            updateCellColor(col, current_module)
            var ix = parseInt(current_module.split("_")[1])

            var inx = sel_inx.indexOf(ix);
            annot_colors[inx] = col

            main_map.render();
            $('#modulecolordlg').dialog('close');
        },
        showInput: true,
        showPalette: true,
        showSelectionPalette: true, // true by default
        palette: [
            ['#ffffff', '#905356'],
            ['#38597a', '#222222']
        ]
    });
    if (heatmapType === "mummichog") {
        $('#dg2').datagrid({
            onSelect: function (index, row) {
                //var nodeIDs = focus_fun_anot[row.pathname];
                var query = $('#queryView').val();
                if (annot_arr.length > 10) {
                    $.messager.alert('Error',"A maximum of 10 pathways can be highlighted at the same time");
                    unselType = "limit"
                    $('#dg2').datagrid("unselectRow", index)
                    return;
                } else {
                    //var hits = hit_num[index]
                    var sig = hit_sig[index]

                    var obj = []
                    var marr = []
                    var carr = []

                    if (peakTable["Retention.Time"] !== undefined) {
                        for (var i = 0; i < hit_num.length; i++) {
                            var inx = getAllIndexes(peakTable["Matched.Compound"], sig[i])
                            if (inx.length > 0) {
                                for (var j = 0; j < inx.length; j++) {
                                    if (sig.indexOf(peakTable["Matched.Compound"][inx[j]]) !== -1) {
                                        obj.push(peakTable["Query.Mass"][inx[j]] + "__" + peakTable["Retention.Time"][inx[j]])
                                        marr.push(peakTable["Matched.Form"][inx[j]] + " | " + peakTable["Matched.Compound"][inx[j]])
                                        carr.push(peakTable["Compound.Name"][inx[j]])
                                    }
                                }
                            }
                        }
                    } else {
                        for (var i = 0; i < hit_num.length; i++) {
                            var inx = getAllIndexes(peakTable["Matched.Compound"], sig[i])
                            if (inx.length > 0) {
                                for (var j = 0; j < inx.length; j++) {
                                    if (sig.indexOf(peakTable["Matched.Compound"][inx[j]]) !== -1) {
                                        obj.push(peakTable["Query.Mass"][inx[j]])
                                        marr.push(peakTable["Matched.Form"][inx[j]] + " | " + peakTable["Matched.Compound"][inx[j]])
                                        //console.log(peakTable["Matched.Compound"][inx[j]] + " " + peakTable["Compound.Name"][inx[j]])
                                        carr.push(peakTable["Compound.Name"][inx[j]])
                                    }

                                }
                            }
                        }
                    }

                    var overallCheck = document.getElementById("viewOverall").checked;

                    if (overallCheck) {
                        if (typeof obj === 'string') { // single element
                            obj = [obj];
                        }
                        path_focus = 1;
                        path_focus_names = obj;
                        setup_focus_view(obj);
                    }

                    if (annot_arr.length < 11) {
                        annot_arr.push(obj)
                        annot_arr_length.push(obj.length)
                        annot_arr2.push(marr)
                        annot_arr3.push(carr)
                        if (!overallCheck) {
                            annot_inx_arr.push(annot_inx)
                            sel_inx.push(index)
                            annot_colors.push(twentyColors[annot_inx])
                        } else {
                            annot_inx_arr.push("")
                            sel_inx.push("")
                            annot_colors.push("NA")
                        }
                    } else {
                        annot_arr.shift()
                        annot_arr.push(obj)
                        annot_inx_arr.shift()
                        annot_inx_arr.push(annot_inx)
                        sel_inx.pop()
                        sel_inx.push(index)
                    }

                    if (!overallCheck) {
                        currColor = twentyColors[annot_inx]
                        updateCellColor(currColor, "function_" + index);
                    }

                    annot_inx++;
                    smpl_map.render();
                    main_map.render();

                }
            },
            onUnselect: function (index, row) {
                if (unselType === "limit") {
                    unselType = "default";
                    return;
                }
                var ix = index
                var inx = sel_inx.indexOf(ix)
                annot_inx_arr.splice(inx, 1)
                annot_arr.splice(inx, 1)
                annot_arr_length.splice(inx, 1)
                annot_arr2.splice(inx, 1)
                sel_inx.splice(inx, 1)
                annot_colors.splice(inx, 1)
                updateCellColor("#ffffff", "function_" + index);
                main_map.render();
                smpl_map.render();

            }
        })
    } else {

        $('#dg2').datagrid({
            onSelect: function (index, row) {
                //var nodeIDs = focus_fun_anot[row.pathname];
                var query = $('#queryView').val();
                var gene_ids = global_fun_anot[row.pathname];
                if (typeof gene_ids === 'string') { // single element
                    gene_ids = [gene_ids];
                }
                var obj = []
                var marr = []
                var carr = []
                var overallCheck = document.getElementById("viewOverall").checked;

                if (!overallCheck) {
                    for (var i = 0; i < gene_ids.length; i++) {
                        var inx = getAllIndexes(names_focus, gene_ids[i])
                        if (inx.length > 0) {
                            for (var j = 0; j < inx.length; j++) {
                                if (gene_ids.indexOf(names_focus[inx[j]]) !== -1) {
                                    obj.push(names_focus[inx[j]]);
                                    marr.push("");
                                    carr.push(names_focus[inx[j]])
                                }

                            }
                        }
                    }
                    if (annot_arr.length < 11) {
                        annot_arr.push(obj)
                        annot_arr_length.push(obj.length)
                        annot_arr2.push(marr)
                        annot_arr3.push(carr)
                        annot_inx_arr.push(annot_inx)
                        sel_inx.push(index)
                        annot_colors.push(twentyColors[annot_inx])
                    } else {
                        annot_arr.shift()
                        annot_arr.push(obj)
                        annot_inx_arr.shift()
                        annot_inx_arr.push(annot_inx)
                        sel_inx.pop()
                        sel_inx.push(index)
                    }

                    currColor = twentyColors[annot_inx]
                    updateCellColor(currColor, "function_" + index);

                    annot_inx++;
                    smpl_map.render();
                    main_map.render();
                } else {

                    path_focus = 1;
                    path_focus_names = gene_ids;
                    setup_focus_view(gene_ids);
                }
                var stats = $("#stats");
                stats.empty();
                var t = "<ul><lh>" + row.pathname + "</lh>";
                for (var i = 0; i < gene_ids.length; i++) {
                    t = t + '<li>' + gene_ids[i] + '</li>';
                }
                t = t + '</ul>';
                stats.append(t);
            },
            onUnselect: function (index, row) {
                if (unselType === "limit") {
                    unselType = "default";
                    return;
                }
                var ix = index
                var inx = sel_inx.indexOf(ix)
                annot_inx_arr.splice(inx, 1)
                annot_arr.splice(inx, 1)
                annot_arr_length.splice(inx, 1)
                annot_arr2.splice(inx, 1)
                sel_inx.splice(inx, 1)
                annot_colors.splice(inx, 1)
                updateCellColor("#ffffff", "function_" + index);
                main_map.render();
                smpl_map.render();

            }
        })

    }

    // data  
    heat.data = function (new_data, init_focus) {
        if (!new_data) {
            return data;
        } else {
            data = new_data;
            //init focus data and sort index
            if (init_focus) {
                data_focus = data.slice(0, init_focus_genes);
                _(gene_cluster).each(function (v, inx) {
                    gene_cluster_focus[inx] = v.slice(0, init_focus_genes);
                });
            }
        }
    };

    heat.names = function (new_data, init_focus) {
        if (!new_data) {
            return names;
        } else {
            names = new_data;
            if (init_focus) {
                names_focus = names.slice(0, init_focus_genes)
            }
        }
    };

    heat.meta = function (new_data) {
        if (!new_data) {
            return meta_data;
        } else {
            meta_data = new_data;
        }
    };

    heat.meta_anot = function (new_data) {
        if (!new_data) {
            return meta_anot;
        } else {
            meta_anot = new_data;
        }
    };

    heat.meta_names = function (new_data) {
        if (!new_data) {
            return meta_names;
        } else {
            meta_names = new_data;
        }
    };

    heat.focus_data = function (new_data) {
        if (!new_data) {
            return data_focus;
        } else {
            data_focus = new_data;
        }
    };

    heat.selected_data = function (new_data) {
        if (!new_data) {
            return selected_data;
        } else {
            selected_data = new_data;
        }
    };

    heat.selected_names = function (new_data) {
        if (!new_data) {
            return selected_names;
        } else {
            selected_names = new_data;
        }
    };

    heat.gene_cluster = function (new_data) {
        if (!new_data) {
            return gene_cluster;
        } else {
            gene_cluster = new_data;
        }
    };

    heat.gene_cluster_focus = function (new_data) {
        if (!new_data) {
            return gene_cluster_focus;
        } else {
            gene_cluster_focus = new_data;
        }
    };

    heat.gene_cluster_names = function (new_data) {
        if (!new_data) {
            return gene_cluster_names;
        } else {
            gene_cluster_names = new_data;
        }
    };

    heat.sample_cluster = function (new_data) {
        if (!new_data) {
            return sample_cluster;
        } else {
            sample_cluster = new_data;
        }
    };

    heat.sample_cluster_names = function (new_data) {
        if (!new_data) {
            return sample_cluster_names;
        } else {
            sample_cluster_names = new_data;
        }
    };
    heat.main_map = function () {
        return main_map;
    }


    $('#resolution').change(function () {
        resolution = parseInt($('#resolution').val());
        if (resolution > 8) {
            init_focus_genes = 50;
        } else if (resolution < 8) {
            init_focus_genes = 110;
        } else {
            init_focus_genes = 70;
        }
        heat.draw();
    });

    $('#border').change(function () {
        border = $('#border').val();
        heat.draw();
    });



    $('#color').change(function () {
        var col = $('#color').val();
        savedState.colorSchema = col;
        updateHeatmapColors(col);
    });

    heat.updateHeatmapColors = function (col) {
        updateHeatmapColors(col);
    }

    function updateHeatmapColors(col) {
        if (col === 'gbr30') {
            options.colors.display = colors_gbr30;
            options.colors.hover = hoverColors_gbr30;
            options.colors.contrast = {...colors_gbr30, ...contrastColors}
        } else if (col === 'byr30') {
            options.colors.display = colors_byr30;
            options.colors.hover = hoverColors_byr30;
            options.colors.contrast = {...colors_byr30, ...contrastColors}
        } else if (col === 'cb') {
            options.colors.display = colors_cb;
            options.colors.hover = hoverColors_cb;
            options.colors.contrast = {...colors_cb, ...contrastColors}
        } else { //nwf30
            options.colors.display = colors_nwf30;
            options.colors.hover = hoverColors_nwf30;
            options.colors.contrast = {...colors_nwf30, ...contrastColors}
        }
        var light = $(parent.window.document).find("#formHidden\\:selectedTheme").val();
        if (light === "light") {
            options.colors.display["hidden"] = "#fff";
        } else {
            options.colors.display["hidden"] = "#000";
        }
        heat.draw();
    }
    $('#viewOpt').change(function () {
        $("#genelist").val('');
        var viewOpt = $('#viewOpt').val();
        if (viewOpt === 'na') {
            return;
        }

        var genenms = '';
        if (viewOpt === "focus") {
            genenms = main_map.nms;
        } else if (viewOpt === "custom") {
            if ($('#results-wrap').length) {
                genenms = selected_map.nms;
            } else {
                $.messager.alert('Error',"Custom view not created yet!");
                return false;
            }
        } else { //overview
            genenms = over_map.nms;
        }
        var textVal = '';
        for (var i = 0; i < genenms.length; i++) {
            textVal = textVal + genenms[i] + '\n';
        }
        $("#genelist").val(textVal);
    });

    $('#peak-cluster').change(function () {
        heat.clustergenes($('#peak-cluster').val());
        if (gene_cluster_names.indexOf($('#peak-cluster').val()) === -1) {
            $.messager.alert('Error', 'This option is not available for current heatmap, please choose another option', 'error');
        }
        $.messager.alert('Information', 'Clustering was performed sucessfully. Please use your mouse to \n\
                                    drag-and-select a region of interest from the heatmap on the left panel.', 'info');

    });

    $('#submitBn').click(function () {//custom signature profiling
        //need to get each line
        var ids = $('#signature').val().split('\n');
        setup_focus_view(ids);
    });

    $('#selBn').click(function () {//copy all genes from overview to focus view
        //var gene_ids = Object.keys(over_map);
        var names = heat.names(0, 0);
        names = names.slice(7, names.length - 7);
        setup_focus_view(names);
        all_in_focus = 1;
        $.messager.alert('OK', 'All features are now in focus view!', 'info');
    });


    $('#builder').click(function () {
        if ($('#results-wrap').length) { //already added
            $.messager.alert('OK', 'Heatmap editor was alreay added! You can drag its border to minimize the size, or reload the page to remove it.', 'info');
        } else {
            $('#mainview').layout('add', {
                id: 'editor',
                region: 'south',
                split: true,
                border: true,
                height: 120
            });
            //now add content to editor
            $("#editor").css('border-color', 'orange');
            $("#editor").append(
                    '<div id="results-wrap" class="heatmap-wrap">' +
                    '<canvas id="results" class="heatmap" height="40"></canvas>' +
                    '<canvas id="reshover" class="hover" height="40"></canvas>' +
                    '</div>' +
                    '<span class="panel_tip"> <a id="editorTip" href="javascript:void(0)">' +
                    '<img src="../../resources/images/tip.png"/></a></span>' +
                    '<span class="menu_label0" id="toFocus" style="opacity: 1.0;">To Focusview</span>' +
                    '<span class="menu_label1" id="addSep" style="opacity: 1.0;">Add separator</span>' +
                    '<span class="menu_label2" id="editSmpl" style="opacity: 1.0;">Edit samples</span>'
                    );

            $('#editorTip').tooltip({
                position: 'right',
                content: 'To add, drag and select from Focus view. ' +
                        'Double click to delete a row; drag and drop to reorganize'
            });

            $('#toFocus').click(function () {
                if (selected_data.length) {
                    setup_focus_view(selected_names);
                }
                selected_names = [];
                selected_data = [];
                selected_map.render();
                $('#editor').empty();
                heat.draw();
                $('#mainview').layout('remove', 'south');
            });

            $('#addSep').click(function () {
                if (selected_data.length) {
                    selected_data.push('');
                    selected_names.push('INVEX-SEPARATOR');
                    selected_map.render();
                }

            });


            $('#editSmpl').click(function () {
                if (selected_data.length) {
                    //set up the data model for custom heatmap
                    setup_sample_editor();
                    //display the dialog
                    $("#smpl_dialog").dialog('open');
                    //can associate functions with button
                    $('#exportImg').unbind("click"); // remove previous one
                    $('#exportImg').click(function () {
                        var imgData = heat.download_image("editor");
                        doc.getElementById("downloadimage").src = imgData;
                        $("#dialog").dialog('open');
                    });
                    editor_map.render();
                }
            });

            //add functions
            $("#editor").scroll(function () {
                var eleft = $("#editor").scrollLeft();
                $("#mainsouth").scrollLeft(eleft);
                $("#maincenter").scrollLeft(eleft);
                $("#mainnorth").scrollLeft(eleft);
                var nleft = $("#mainnorth").scrollLeft();
                var cleft = $("#maincenter").scrollLeft();
                var sleft = $("#mainsouth").scrollLeft();
                var minleft = Math.min(sleft, nleft, cleft);
                $("#mainnorth").scrollLeft(minleft);
                $("#maincenter").scrollLeft(minleft);
                $("#mainsouth").scrollLeft(minleft);
                $("#editor").scrollLeft(minleft);
            });
            heat.draw();//updating main heatmap, 
            $.messager.alert('OK', 'You can now double click or drag to select genes from Focus View to Heatmap Builder.', 'info');
        }
    });
    heat.setup_focus_view = function (id_arry) {
        setup_focus_view(id_arry);
    }
    //setup focus view with array of ids
    function setup_focus_view(id_arry) {
        var myfocus_data = [];
        var myfocus_name = [];
        //init a 2D array same len as gene cluster
        var myfocus_gene_cluster = [];
        _(gene_cluster).each(function (v, inx) {
            myfocus_gene_cluster.push([]);
        });
        var unmatched_names = '';

        for (var i = 0; i < id_arry.length; i++) {
            //alert(id_arry[i]);
            var matchString = id_arry[i].toLowerCase();
            var hit_inx = -1;
            $.each(names, function (index, value) {
                if (hit_inx === -1 && value.toLowerCase().includes(matchString)) {
                    hit_inx = index;
                    return false;
                }
            });

            //var hit_inx = _.indexOf(mynames, id_arry[i].toLowerCase())
            if (hit_inx > -1) {
                myfocus_name.push(names[hit_inx]);
                myfocus_data.push(data[hit_inx]);
                _(gene_cluster).each(function (v, inx) {
                    myfocus_gene_cluster[inx].push(v[hit_inx]);
                });

            } else {
                unmatched_names = unmatched_names + ' ' + id_arry[i];
            }
        }
        if (myfocus_name.length > 0) {
            data_focus = myfocus_data;
            names_focus = myfocus_name;
            gene_cluster_focus = myfocus_gene_cluster;
            main_map.update(data_focus, myfocus_name);
            main_map.render();
            if (unmatched_names !== '') {
                // alert("Unmatched IDs:" + unmatched_names);
            }
        } else {
            $.messager.alert('Status', 'No matches found!', 'info');
        }
    }


    $('#advanced').change(function () {
        var key = $(this).val();
        if (key !== 'NA') {
            if (key === 'download') {
                var res_dir = $.ajax({
                    type: "GET",
                    url: '/MetaboAnalyst/faces/AjaxCall?function=download' + "&ignoreMe=" + new Date().getTime(),
                    async: false
                }).responseText;
                $("#fileLnk").empty();
                $("#fileLnk").append("The results from data analysis and annotation can be downloaded here:" +
                        "<a href='" + res_dir + "'><b><u>Download.zip</u><b></a>");
                $("#filedialog").dialog('open');
            } else {
                const imgData = heat.download_image(key);
                doc.getElementById("downloadimage").src = imgData;
                $("#dialog").dialog('open');

            }
        }
    });
    $('#resetFocusBn2').click(function () {
        annot_arr = [];
        annot_arr_length = [];
        annot_arr2 = [];
        annot_arr3 = [];
        annot_inx_arr = [];
        sel_inx = [];
        annot_colors = []
        $('#dg2').datagrid('clearSelections');
        var dat = $('#dg2').datagrid('getData');
        for (var i = 0; i < dat["total"]; i++) {
            updateCellColor("#ffffff", "function_" + i);
        }
        setup_focus_view(names_focus_reset);
    });

    $('#resetFocusBn').click(function () {
        // var gene_ids = global_gene_anot[names_focus[0]][0];
        path_focus = 0;
        if (current_enr_names !== "NA") {
            setup_focus_view(current_enr_names);
        } else {
            setup_focus_view(heat.names(0, 0));
            all_in_focus = 1;
        }
        //main_map.update(data_focus, names_focus);
        //main_map.render();
        // alert("done!");
    });

    $('#dg').datagrid({
        onDblClickCell: function (index, field, value) {
            var gene_ids = global_fun_anot[value];
            if (typeof gene_ids === 'string') { // single element
                gene_ids = [gene_ids];
            }
            path_focus = 1;
            path_focus_names = gene_ids;
            setup_focus_view(gene_ids);
            var stats = $("#stats");
            stats.empty();
            var t = "<ul><lh>" + value + "</lh>";
            for (var i = 0; i < gene_ids.length; i++) {
                t = t + '<li>' + gene_ids[i] + '</li>';
            }
            t = t + '</ul>';
            stats.append(t);
        }
    });

    $('#dg1').datagrid({
        onDblClickCell: function (index, field, value) {
            //alert(index + " " + field + " " + value);
            var gene_ids = focus_fun_anot[value];
            if (typeof gene_ids === 'string') { // single element
                gene_ids = [gene_ids];
            }
            setup_focus_view(gene_ids);
        }
    });

    heat.download_image = function (view) {
        takingImage = true;

        if (view === "heatmap") {
            main_map.render();
            takingImage = false;
        }
        var canvas = doc.getElementById("canvas2image");
        var context = canvas.getContext("2d");
        var meta_source, map_source, smpl_source, h, h1, h2, h3, w;
        var padding_w = 10;
        var padding_h = 10;

        if (view === 'overview') { //overview 
            meta_source = doc.getElementById("overmetabar");
            map_source = doc.getElementById("overview");
            h1 = $("#overmetabar").height();
            h2 = $("#overview").height();
            h = h1 + h2 + 25 + padding_h;
            w = $("#overview").width() + padding_w * 2;

            if (isRetinaDisplay()) {
                padding_w = padding_w * window.devicePixelRatio;
                padding_h = padding_h * window.devicePixelRatio;
                h1 = h1 * window.devicePixelRatio;
                h2 = h2 * window.devicePixelRatio;
                w = w * window.devicePixelRatio;
                h = h * window.devicePixelRatio;
            }

            $('#canvas2image').attr('height', h);
            $('#canvas2image').attr('width', w);

            context.fillStyle = bgColor;
            context.fillRect(0, 0, w, h);
            context.translate(padding_w, padding_h);
            context.drawImage(meta_source.getContext('2d').canvas, 0, 0);
            context.drawImage(map_source.getContext('2d').canvas, 0, h1 + 5);

            //add color legend 
            //first need to center the legend (exclude padding)
            w = w - padding_w * 2;
            h = h - padding_h;
            var center = Math.floor(w / 2);
            var left_start = center - 40;
            context.fillStyle = textColor;
            context.font = 6 + 'pt Arial';
            context.fillText('Low', left_start - 20, h - 8);
            context.fillText('High', center + 50, h - 8);
            for (var i = 1; i < 30; i++) {
                context.fillStyle = options.colors.display[i];
                context.fillRect(left_start + i * 4, h - 12, 4, 4);
            }

        } else if ($('#' + view).length) {

            meta_source = doc.getElementById("metabar");
            map_source = doc.getElementById(view);
            smpl_source = doc.getElementById("smpls");

            h1 = $('#metabar').height();
            h2 = $('#' + view).height();
            h3 = $('#smpls').height();
            w = $('#metabar').width() + padding_w * 2;
            w = mainMapWidth - 50
            h = h1 + h2 + h3 + 25 + padding_h;

            if (isRetinaDisplay()) {
                padding_w = padding_w * window.devicePixelRatio;
                padding_h = padding_h * window.devicePixelRatio;
                h1 = h1 * window.devicePixelRatio;
                h2 = h2 * window.devicePixelRatio;
                h3 = h3 * window.devicePixelRatio;
                w = w * window.devicePixelRatio;
                h = h * window.devicePixelRatio;
            }

            $('#canvas2image').attr('height', h);
            $('#canvas2image').attr('width', w);

            context.fillStyle = bgColor;
            context.fillRect(0, 0, w, h);
            context.translate(padding_w, padding_h);
            context.drawImage(meta_source.getContext('2d').canvas, 0, 0);
            context.drawImage(map_source.getContext('2d').canvas, 0, h1 + 10);
            context.drawImage(smpl_source.getContext('2d').canvas, 0, h1 + h2 + 15);

        } else {
            $.messager.alert('Error',"You must first create a custom heatmap using the Heatmap builder!");
            return;
        }
        //need to add meta and figure legend
        var imgData = canvas.toDataURL('image/png');
        return imgData;
    }
    ;

    heat.clustersamples = function (sort_key) {

        var grp_lbls;
        if (cluster_methods.indexOf(sort_key) > -1) {
            grp_lbls = sample_cluster[_.indexOf(sample_cluster_names, sort_key)];
        } else {
            grp_lbls = meta_data[_.indexOf(meta_names, sort_key)];
        }
        var all_data = data.concat(data_focus, selected_data, meta_data, sample_cluster, [sample_names]);
        all_data = sort2darrayByCol(grp_lbls, all_data);

        //now update individual data
        var data_start = 0;
        var data_end = data.length;
        var sorted_data = all_data.slice(data_start, data_end);
        heat.data(sorted_data, 0);

        data_start = data_end;
        data_end = data_end + data_focus.length;
        var sorted_data_focus = all_data.slice(data_start, data_end);
        heat.focus_data(sorted_data_focus);

        data_start = data_end;
        data_end = data_end + selected_data.length;
        var sorted_selected_data = all_data.slice(data_start, data_end);
        heat.selected_data(sorted_selected_data);

        data_start = data_end;
        data_end = data_end + meta_data.length;
        var sorted_meta_data = all_data.slice(data_start, data_end);
        heat.meta(sorted_meta_data);

        data_start = data_end;
        data_end = data_end + sample_cluster.length;
        var sorted_sample_cluster = all_data.slice(data_start, data_end);
        heat.sample_cluster(sorted_sample_cluster);

        data_start = data_end;
        data_end = all_data.length;
        sample_names = all_data.slice(data_start, data_end)[0];
    };


    heat.clustergenes = function (sort_key) {
        //sort based on gene cluster info, synchronize all associated info
        //console.log(sort_key);
        var clustinx = _.indexOf(gene_cluster_names, sort_key);
        //console.log(clustinx);

        //sort overall view
        //note: names is a vector, need to put [] to retain as an array with 1 element

        var all_data = sort2darrayByRow(clustinx, data, gene_cluster, [names]);

        //obtain sorted data
        var sorted_data = all_data[0];
        var sorted_gene_cluster = all_data[1];
        var sorted_names = all_data[2][0];     //array with one element (also array), need to extract as a vector

        //update data
        heat.gene_cluster(sorted_gene_cluster);
        heat.data(sorted_data, 0);
        heat.names(sorted_names, 0);
        over_map.update(sorted_data, sorted_names);
        over_map.render();

        //remove current annotatin and pathways
        clearAnnot();
        $("#dg2").datagrid("loadData", []);

        //empty the current focus view
        names_focus = [];
        data_focus = [];

        main_map.update([], []);
        main_map.render();
    };

    heat.clusterfocus = function (sort_key) {

        var clustinx = _.indexOf(gene_cluster_names, sort_key);

        //sort overall view
        //note: names is a vector, need to put [] to retain as an array with 1 element
        var all_data = sort2darrayByRow(clustinx, data, gene_cluster, [names]);

        //obtain sorted data
        var sorted_data = all_data[0];
        var sorted_gene_cluster = all_data[1];
        var sorted_names = all_data[2][0];     //array with one element (also array), need to extract as a vector

        //update data
        heat.gene_cluster(sorted_gene_cluster);
        heat.data(sorted_data, 0);
        heat.names(sorted_names, 0);
        over_map.update(sorted_data, sorted_names);
        over_map.render();

        //sort current focus view
        if (all_in_focus) {
            setup_focus_view(heat.names(0, 0));
        } else {
            var my_names;
            if (path_focus) {
                my_names = path_focus_names;
            } else {
                my_names = names_focus;
            }

            var all_data2 = sort2darrayByRow(clustinx, data_focus, gene_cluster_focus, [my_names]);
            //update with sorted data
            data_focus = all_data2[0];
            gene_cluster_focus = all_data2[1];
            my_names = all_data2[2][0]; //array with one element (also array), need to extract as a vector
            //re-order
            if (path_focus) {
                path_focus_names = my_names;
            } else {
                names_focus = my_names;
            }

            main_map.update(data_focus, my_names);
            main_map.render();
        }
    }

    heat.draw = function () {

        overmeta_map = heatmap('overmetabar', 'overmetahover', meta_data, data[0].length, meta_names, sample_names, {
            colorize: function (val) {
                return options.colors.contrast[val];
            },
            highlight: function (val) {
                return options.colors.contrast[val];
            },
            dotheight: 4,
            dotwidth: 1,
            gutsize: 0,
            totheight: 4,
            totwidth: 1,
            viewid: 1
        });
        overmeta_map.set_metaanot(meta_anot); //set meta 

        //overview heatmap with resolution 1
        over_map = heatmap('overview', 'overhover', data, data[0].length, names, sample_names, {
            colorize: function (val) {
                return options.colors.display[val];
            },
            highlight: function (val) {
                return options.colors.hover[val];
            },
            dotheight: 1,
            dotwidth: 1,
            gutsize: 0,
            totheight: 1,
            totwidth: 1,
            viewid: 2
        });

        var over_drag_flag = 0;
        var start_row = 0;
        over_map.hover.onmousedown = function (e) {
            clearTimeout(timer);
            timer = setTimeout(function () {
                var pos = indices(1, 1, 0, e);
                var start_y_pos = e.pageY - $(e.target).offset().top;
                over_map.setbatchmode(1, start_y_pos);
                over_drag_flag = 1;
                start_row = pos.i;
                if (start_row < 7) {
                    start_row = 7
                }
                over_map.hover.addEventListener("mouseup", over_mouseup);
            }, 250);
        };

        function over_mouseup(e) {
            if (over_drag_flag) {
                over_map.hover.removeEventListener("mouseup", over_mouseup);
                clearTimeout(timer);
                over_drag_flag = 0;
                over_map.setbatchmode(0, 0);
                //now update the main map
                var pos = indices(1, 1, 0, e);
                var end_row = pos.i;
                if (end_row > padded_data.length - 7) {
                    end_row = padded_data.length - 7;
                }
                data_focus = data.slice(start_row, end_row);
                names_focus = names.slice(start_row, end_row);
                path_focus = 0;
                _(gene_cluster).each(function (v, inx) {
                    gene_cluster_focus[inx] = v.slice(start_row, end_row);
                });

                all_in_focus = 0;
                main_map.update(data_focus, names_focus);
                main_map.render();
                clearAnnot();
                $("#dg2").datagrid("loadData", []);
            }
        }

        meta_map = heatmap('metabar', 'metahover', meta_data, data[0].length, meta_names, sample_names, {
            colorize: function (val) {
                return options.colors.contrast[val];
            },
            highlight: function (val) {
                return options.colors.contrast[val];
            },
            dotheight: options.size.dotheight(),
            dotwidth: options.size.dotwidth(),
            gutsize: options.size.gutsize(),
            totheight: options.size.totheight(),
            totwidth: options.size.totwidth(),
            viewid: 3
        });
        meta_map.set_metaanot(meta_anot); //set meta 
        meta_map.hover.onclick = function (e) {
            if (timer)
                clearTimeout(timer);
            timer = setTimeout(function () {
            }, 250);
        }
        //double click to sort samples (all views change)
        meta_map.hover.ondblclick = function (e) {
            clearTimeout(timer);
            var pos = indices(meta_map.totwidth(), options.size.totheight(), options.size.gutsize(), e);
            var sort_key = meta_names[pos.i];
            heat.clustersamples(sort_key);
            sample_sort_key = sort_key;
            heat.draw();
        };

        //main heatmap
        main_map = heatmap('heatmap', 'hover', data_focus, data[0].length, names_focus, sample_names, {
            colorize: function (val) {
                return options.colors.display[val];
            },
            highlight: function (val) {
                return options.colors.hover[val];
            },
            dotheight: options.size.dotheight(),
            dotwidth: options.size.dotwidth(),
            gutsize: options.size.gutsize(),
            totheight: options.size.totheight(),
            totwidth: options.size.totwidth(),
            viewid: 4
        });

        main_map.hover.onclick = function (e) {
            clearTimeout(timer);
            timer = setTimeout(function () {
            }, 250);
        }

        //selected profile
        //console.log($('#results-wrap').length + "dddddd")
        if ($('#results-wrap').length) {//test if exists
            selected_map = heatmap('results', 'reshover', selected_data, data[0].length, selected_names, sample_names, {
                colorize: function (val) {
                    return options.colors.display[val];
                },
                highlight: function (val) {
                    return options.colors.hover[val];
                },
                dotheight: options.size.dotheight(),
                dotwidth: options.size.dotwidth(),
                gutsize: options.size.gutsize(),
                totheight: options.size.totheight(),
                totwidth: options.size.totwidth(),
                viewid: 5
            });
            selected_map.hover.onclick = function (e) {
                if (timer)
                    clearTimeout(timer);
                timer = setTimeout(function () {
                }, 250);
            }

            //no render, since no data at beginning    
            selected_map.hover.ondblclick = function (e) { //double click remove the row
                clearTimeout(timer);
                var pos = indices(selected_map.totwidth(), options.size.totheight(), options.size.gutsize(), e);
                selected_data.splice(pos.i, 1);
                selected_names.splice(pos.i, 1);
                selected_map.update(selected_data, selected_names);
                selected_map.render();
            };

            var selected_drag_inx;
            var selected_drag_flag = 0;

            selected_map.hover.onmousedown = function (e) {
                clearTimeout(timer);
                timer = setTimeout(function () {
                    var pos = indices(selected_map.totwidth(), options.size.totheight(), options.size.gutsize(), e);
                    selected_drag_flag = 1;
                    selected_drag_inx = pos.i;
                    selected_map.setdragmode(1);
                    selected_map.hover.addEventListener("mouseup", selected_mouseup);
                }, 250);
            }

            //drag-and-drop
            function selected_mouseup(e) {
                if (selected_drag_flag) {
                    var pos = indices(selected_map.totwidth(), options.size.totheight(), options.size.gutsize(), e);
                    var insert_inx = pos.i; //insert  and push current below

                    var drag_row = selected_data[selected_drag_inx];
                    var drag_gene = selected_names[selected_drag_inx];

                    //first remove 
                    selected_data.splice(selected_drag_inx, 1);
                    selected_names.splice(selected_drag_inx, 1);

                    //assign to new place
                    selected_data.splice(insert_inx, 0, drag_row);
                    selected_names.splice(insert_inx, 0, drag_gene);

                    selected_map.update(selected_data, selected_names);
                    selected_map.render();
                    selected_map.hover.removeEventListener("mouseup", selected_mouseup);
                    clearTimeout(timer);
                    selected_map.setdragmode(0)
                    selected_drag_flag = 0;
                }
            }

            //=== main map function only meaningful when editor is added
            main_map.hover.ondblclick = function (e) {
                clearTimeout(timer);
                //update selected data
                var pos = indices(main_map.totwidth(), options.size.totheight(), options.size.gutsize(), e);
                var inx = $.inArray(data_focus[pos.i], selected_data);
                if (inx === -1) {
                    //append to the last
                    selected_data.push(data_focus[pos.i]);
                    selected_names.push(names_focus[pos.i]);
                    selected_map.update(selected_data, selected_names);
                    selected_map.render();
                } else {
                    $.messager.alert('Error',"The item is already included at row " + (inx + 1))
                }
            };

            //allow selec multiple consecutive rows
            var main_drag_flag = 0;
            var start_drag_inx = 0;
            var end_drag_inx = 0;
            main_map.hover.onmousedown = function (e) {
                clearTimeout(timer);
                timer = setTimeout(function () {
                    var pos = indices(main_map.totwidth(), options.size.totheight(), options.size.gutsize(), e);
                    var start_y_pos = e.pageY - $(e.target).offset().top;
                    main_map.setbatchmode(1, start_y_pos);
                    main_drag_flag = 1;
                    start_drag_inx = pos.i;
                    main_map.hover.addEventListener("mouseup", main_mouseup);
                }, 250);
            }

            function main_mouseup(e) {
                if (main_drag_flag) {
                    var pos = indices(main_map.totwidth(), options.size.totheight(), options.size.gutsize(), e);
                    var end_drag_inx = pos.i; //insert  and push current below

                    var arry = data_focus.slice(Math.min(start_drag_inx, end_drag_inx), Math.max(start_drag_inx, end_drag_inx) + 1);
                    var nms = names_focus.slice(Math.min(start_drag_inx, end_drag_inx), Math.max(start_drag_inx, end_drag_inx) + 1);

                    //append to the last, need to do it one-by-one
                    //so that duplicate will be ignored
                    var dupgenes = '';
                    for (var i = 0, l = nms.length; i < l; i++) {
                        var inx = $.inArray(nms[i], selected_names);
                        if (inx === -1) {
                            //append to the last
                            selected_data.push(arry[i]);
                            selected_names.push(nms[i]);
                        } else {
                            dupgenes = dupgenes + ' ' + nms[i];
                        }
                    }
                    if (dupgenes !== '') {
                        $.messager.alert('Error',dupgenes + ' are ignored. They have already been included in your previous selections.')
                    }

                    main_map.hover.removeEventListener("mouseup", main_mouseup);
                    clearTimeout(timer);
                    main_drag_flag = 0;
                    main_map.setbatchmode(0, 0);
                    selected_map.render();
                }
            }
        }

        //sample labels map
        smpl_map = {};
        smpl_map.render = function () {
            var ctx = doc.getElementById('smpls').getContext('2d');
            var width = sample_names.length * options.size.totwidth() + options.size.gutsize();
            var height = 40;
            var font_size = 0;
            var text_width = 0;
            var line_width = options.size.totwidth();
            if (width < 400) {//min img width 400 pixel (not include text)
                line_width = Math.floor(400 / sample_names.length);
                width = sample_names.length * line_width;
            }
            if (resolution <= 8) {
                font_size = 7;
                height = 80;
                if (resolution === 8) {
                    text_width = 80;
                }
            } else if (resolution === 12) {
                font_size = 9;
                height = 100;
                text_width = 100;
            } else {
                font_size = 12;
                height = 120;
                text_width = 120;
            }


            width = mainMapWidth
            $('#smpls').attr('height', height);
            $('#smpls').attr('width', width);
            if (isRetinaDisplay()) {
                var hidefCanvasWidth = $('#smpls').attr('width');
                var hidefCanvasHeight = $('#smpls').attr('height');
                var hidefCanvasCssWidth = hidefCanvasWidth;
                var hidefCanvasCssHeight = hidefCanvasHeight;

                $('#smpls').attr('width', hidefCanvasWidth * window.devicePixelRatio);
                $('#smpls').attr('height', hidefCanvasHeight * window.devicePixelRatio);
                $('#smpls').css('width', hidefCanvasCssWidth);
                $('#smpls').css('height', hidefCanvasCssHeight);
                ctx.scale(window.devicePixelRatio, window.devicePixelRatio);
            }

            var p = $('#main-map').layout('panel', 'south');
            var pane_height = height + 20;
            if (resolution > 8) {
                pane_height = height + 36; //need to acount for scroll bar
            }
            p.panel('resize', {
                height: pane_height
            });

            if (line_width > 8) {
                //calclulate padding so sample label in middle
                var padding = Math.floor((line_width - font_size) / 2);
                if (padding < 0) {
                    padding = options.size.gutsize();
                }
                ctx.save();
                ctx.translate(options.size.gutsize(), options.size.gutsize())
                ctx.rotate(0.5 * Math.PI);
                ctx.fillStyle = textColor;
                ctx.font = font_size + 'pt Arial';
                _(sample_names).each(function (val, i) {
                    ctx.fillText(val, 0, -(line_width * i + padding), height - 20)
                });

                _(sel_inx).each(function (val, i) {
                    ctx.fillText("P" + val, 0, -(line_width * sample_names.length) - path_nm_padding * (i + 0.5), height - 20)
                });
                ctx.restore();
            } else {
                ctx.clearRect(0, 0, width, height);
            }

            //add color legend under the sample names
            //first need to center the legend
            var center = Math.floor((width - text_width) / 2);
            var left_start = center - resolution * 15;
            ctx.fillStyle = textColor;
            ctx.font = font_size + 'pt Arial';
            ctx.fillText('Low', left_start - 20, height - 6);
            ctx.fillText('High', center + resolution * 15 + 10, height - 6);
            for (var i = 1; i < 30; i++) {
                ctx.fillStyle = options.colors.display[i];
                ctx.fillRect(left_start + i * resolution, height - 15, resolution, resolution);
            }
            ;
        }

        $('#loading').hide();
        overmeta_map.render();
        $('#overmeta-wrap').fadeIn();
        over_map.render();
        $('#overview-wrap').fadeIn();
        meta_map.render();
        $('#meta-wrap').fadeIn();
        main_map.render();
        $('#heatmap-wrap').fadeIn();
        smpl_map.render();
        $('#samples-wrap').fadeIn();
        //only render when there is data 
        if (selected_data.length) {
            selected_map.render();
            $('#results-wrap').fadeIn();
        }
        //need to resize layout
        $('#main-map').layout('resize');
    };
    return heat;
}
;

function isRetinaDisplay() {
    if (window.matchMedia) {
        var mq = window.matchMedia("only screen and (min--moz-device-pixel-ratio: 1.3), only screen and (-o-min-device-pixel-ratio: 2.6/2), only screen and (-webkit-min-device-pixel-ratio: 1.3), only screen  and (min-device-pixel-ratio: 1.3), only screen and (min-resolution: 1.3dppx)");
        return (mq && mq.matches || (window.devicePixelRatio > 1));
    }
}

function heatmap(canvas_id, hover_id, data, col_num, nms, smpl_nms, options) {
    var self = {};
    var opts = options;
    var height, width;
    self.data = data;
    self.col_num = col_num;
    self.nms = nms;
    self.sample_nms = smpl_nms;
    self.batchmode = 0; //batch select
    self.dragmode = 0;
    self.start_y_pos = 0;
    self.canvas = doc.getElementById(canvas_id);
    self.ctx = self.canvas.getContext('2d');
    self.hover = doc.getElementById(hover_id);
    self.hoverctx = self.hover.getContext('2d');
    self.colorize = opts.colorize;
    self.highlight = opts.highlight;
    //identifiers for all heatmaps implemented (5) on the page
    //1:overview meta; 2:overview:main; 3:focus: meta; 4: focus: main; 5: builder
    self.viewid = opts.viewid;

    self.render = function () {
        path_nm_padding = opts.totheight
        height = self.data.length * opts.totheight + opts.gutsize;//margin around the image
        width = self.col_num * opts.totwidth + opts.gutsize;
        var text_width = 0;
        var font_size = 0;
        if (self.viewid < 3) { //overview maps (meta or main)
            if (width < 160) {
                width = 160; //min width 160 
                opts.dotwidth = opts.totwidth = Math.floor(160 / self.col_num);
                width = opts.totwidth * self.col_num;
            }
        } else {
            //min image width 400 pixel
            if (width < 400) {//make sure no empty data
                if (self.data.length > 0) {
                    opts.totwidth = Math.floor(400 / self.col_num);
                    opts.dotwidth = opts.totwidth - opts.gutsize;
                    width = self.col_num * opts.totwidth + opts.gutsize;
                } else { // empty data
                    width = 0;
                }
            }
            //when need text labels, need 
            //add length for text labeling (max 16 char)
            if (resolution > 4) {
                if (resolution === 8) {
                    font_size = 7;
                    text_width = 80;
                } else if (resolution === 12) {
                    font_size = 9;
                    text_width = 100;
                } else {
                    //  font_size = 12;
                    //  text_width = 120;
                    font_size = 20;
                    text_width = 120;
                }

                if (annot_arr_length.length > 1) {
                    if (!takingImage) {
                        annot_track_length = Math.max(...annot_arr_length) * 7
                    } else {
                        annot_track_length = Math.max(...annot_arr_length) * 5
                    }
                    text_width = text_width + parseInt(annot_track_length);
                } else {
                    text_width = 220
                }
                width = width + text_width;
                mainMapWidth = width
            }
        }

        $('#' + canvas_id).attr('height', height);
        $('#' + canvas_id).attr('width', width);
        $('#' + hover_id).attr('height', height);
        $('#' + hover_id).attr('width', width);
        if (self.viewid === 3) {
            var p = $('#main-map').layout('panel', 'north');
            p.panel('resize', {
                height: height + 30
            });
        }

        if (isRetinaDisplay()) {
            var hidefCanvasWidth = $('#' + canvas_id).attr('width');
            var hidefCanvasHeight = $('#' + canvas_id).attr('height');
            var hidefCanvasCssWidth = hidefCanvasWidth;
            var hidefCanvasCssHeight = hidefCanvasHeight;

            $('#' + canvas_id).attr('width', hidefCanvasWidth * window.devicePixelRatio);
            $('#' + canvas_id).attr('height', hidefCanvasHeight * window.devicePixelRatio);
            $('#' + canvas_id).css('width', hidefCanvasCssWidth);
            $('#' + canvas_id).css('height', hidefCanvasCssHeight);
            self.ctx.scale(window.devicePixelRatio, window.devicePixelRatio);
        }
        //do not proceed if no data
        if (self.data === undefined || self.data.length == 0) {
            return;
        }
        //clear canvas
        self.ctx.fillStyle = '#333'; // set canvas background color
        self.ctx.fillRect(0, 0, width - text_width, height);  // now fill the canvas 
        self.ctx.translate(opts.gutsize, opts.gutsize);

        //do actual plotting
        if (self.viewid < 3) {
            _(self.data).each(function (row, j) {
                _(row).each(function (val, i) {
                    self.ctx.fillStyle = self.colorize(val);
                    self.ctx.fillRect(opts.totwidth * i, opts.totheight * j, opts.dotwidth, opts.dotheight);
                });
            });
        } else {
            if (resolution > 4) {
                var length_arr = []

                for (var i = 0; i < self.nms.length; i++) {
                    length_arr.push(self.nms[i].length);
                }
                let sum = length_arr.reduce((previous, current) => current += previous);
                let avg_length = sum / length_arr.length;
                self.ctx.font = font_size + 'pt Arial';
                _(self.data).each(function (row, j) {
                    _(row).each(function (val, i) {
                        self.ctx.fillStyle = self.colorize(val);
                        self.ctx.fillRect(opts.totwidth * i, opts.totheight * j, opts.dotwidth, opts.dotheight);
                    });
                    var nmj = self.nms[j]
                    var annotnm;
                    for (var n = 0; n < annot_arr.length; n++) {
                        for (var m = 0; m < annot_arr[n].length; m++) {
                            var nm = annot_arr[n][m];

                            if (nm === self.nms[j]) {

                                nmj = self.nms[j]
                                if (n + 1 === annot_arr.length) {
                                    var inx = getAllIndexes(annot_arr[n], self.nms[j])
                                    if (inx.length > 1) {
                                        var num = inx.length - 1
                                        annotnm = annot_arr2[n][m] + " + " + num + " more";
                                    } else {
                                        annotnm = annot_arr2[n][m]
                                    }
                                }
                                if (annot_colors[n] !== "NA") {
                                    self.ctx.fillStyle = annot_colors[n];
                                    self.ctx.fillRect(opts.totwidth * (row.length) + opts.totheight * (n + 0.5), opts.totheight * j, opts.totheight, opts.dotheight);
                                }
                            }
                        }
                    }

                    self.ctx.fillStyle = textColor;
                    var additional_length = 0
                    if (document.getElementById("viewOverall").checked) {
                        additional_length = 0
                    } else {
                        if (annot_arr.length > 0) {
                            additional_length = annot_arr.length + 1
                        }
                    }

                    self.ctx.fillText(nmj, opts.totwidth * (row.length) + opts.totheight * (additional_length) + opts.gutsize * 2, opts.totheight * (j + 1) - opts.gutsize * 2);
                    if (annot_arr2.length > 0 && annotnm && !takingImage) {
                        self.ctx.fillStyle = textColor;
                        self.ctx.fillText(annotnm, opts.totwidth * (row.length) + opts.totheight * (additional_length + avg_length / 1.2) + opts.gutsize * 2, opts.totheight * (j + 1) - opts.gutsize * 2);
                        annotnm = "";
                    }
                });
            } else {

                _(self.data).each(function (row, j) {
                    if (self.nms[j] === 'INVEX-SEPARATOR') {
                        self.ctx.fillStyle = 'white';
                        self.ctx.fillRect(-opts.gutsize, opts.totheight * j, opts.totwidth * (self.col_num + 1), opts.dotheight);
                    } else {
                        _(row).each(function (val, i) {
                            self.ctx.fillStyle = self.colorize(val);
                            self.ctx.fillRect(opts.totwidth * i, opts.totheight * j, opts.dotwidth, opts.dotheight);
                        });
                    }
                });
            }
        }
    };

    self.hover.onmousedown = function (e) {
        var stats = $("#stats");
        var pos = indices(opts.totwidth, opts.totheight, opts.gutsize, e);
        var val = lookup({
            i: pos.i,
            j: pos.j
        }, self.data);

        if (self.viewid !== 1 && self.viewid !== 3) { //not meta
            stats.empty();
            self.hoverctx.clearRect(0, 0, parseInt(width), parseInt(height));
            self.hoverctx.translate(opts.gutsize, opts.gutsize);
            _(self.data[pos.i]).each(function (val, j) { //highlight whole row
                self.hoverctx.fillStyle = self.highlight(val);
                self.hoverctx.fillRect(opts.totwidth * j, opts.totheight * pos.i, opts.dotwidth, opts.dotheight);
            });
            self.hoverctx.translate(-opts.gutsize, -opts.gutsize);
            var gene_nm = self.nms[pos.i];
            var entrez = global_gene_anot[gene_nm][0];
            var details = global_gene_anot[gene_nm][1];
            var pval = global_gene_anot[gene_nm][2];
            pval = pval - 7
            if (typeof gene_nm === "undefined") {
                gene_nm = "";
                details = "";
                pval = "";
            }
            stats.append('<strong>ID</strong>: ' + gene_nm + '<br/>');
            stats.append('<strong>P-val</strong>: ' + pval + '<br/>');
            if (containsTstat) {
                var tstat = global_gene_anot[gene_nm][3];
                tstat = tstat - 7;
                stats.append('<strong>T-stat</strong>: ' + tstat + '<br/>');
            }

            var annotnm = [];
            for (var n = 0; n < annot_arr.length; n++) {

                for (var m = 0; m < annot_arr[n].length; m++) {
                    var nm = annot_arr[n][m];
                    if (nm === gene_nm) {
                        var inx = getAllIndexes(annot_arr[n], gene_nm)
                        if (inx.length > 1) {
                            for (var k = inx.length - 1; k >= 0; k--) {
                                if (!annotnm.includes(annot_arr2[n][inx[k]])) {
                                    if (annot_arr3[n][inx[k]] !== 0 && annot_arr3[n][inx[k]] !== undefined) {
                                        annotnm = annotnm + annot_arr2[n][inx[k]] + " | " + annot_arr3[n][inx[k]] + '<br/>'
                                    } else {
                                        annotnm = annotnm + annot_arr2[n][inx[k]] + '<br/>'
                                    }
                                }
                            }
                        } else {
                            if (annot_arr3[n][m] !== 0 && annot_arr3[n][inx[k]] !== undefined) {
                                annotnm = annot_arr2[n][m] + " | " + annot_arr3[n][m];
                            } else {
                                annotnm = annot_arr2[n][m]
                            }
                        }

                    }
                }
            }
            if (annotnm.length > 0) {
                stats.append('<strong>Matched Compounds</strong>:<br/>');
            }
            stats.append(annotnm);
        }
    }

    self.hover.onmousemove = function (e) {
        if (self.batchmode) {
            e.target.style.cursor = 'default';
            var yp = e.pageY - $(e.target).offset().top;
            var w = self.col_num * opts.totwidth;
            var y = Math.min(self.start_y_pos, yp);
            var h = Math.abs(yp - self.start_y_pos);
            self.hoverctx.clearRect(0, 0, parseInt(width), parseInt(height));
            self.hoverctx.fillStyle = "rgba(255, 255, 255, 0.5)";
            self.hoverctx.strokeStyle = "rgba(255, 255, 255, 0.7)";
            self.hoverctx.lineWidth = 1;
            self.hoverctx.strokeRect(0, y, w, h);
            self.hoverctx.fillRect(0, y, w, h);
        } else if (self.dragmode) {

            e.target.style.cursor = 'context-menu';
        } else {
            var pos = indices(opts.totwidth, opts.totheight, opts.gutsize, e);
            var val = lookup({
                i: pos.i,
                j: pos.j
            }, self.data);
            if (self.viewid !== 1 && self.viewid !== 3) { //not meta

            } else { // meta
                var stats = $("#stats");
                stats.empty();

                var smpl_nm = self.sample_nms[pos.j];
                var meta_nm = self.nms[pos.i];
                var meta_anot = self.metaanot[val];
                if (typeof smpl_nm === "undefined") {
                    smpl_nm = "";
                }
                if (typeof meta_nm === "undefined") {
                    meta_nm = "";
                }
                if (typeof meta_anot === "undefined") {
                    meta_anot = "";
                }
                stats.append('<li><strong>Sample</strong>: ' + smpl_nm + '</li>');
                stats.append('<li><strong>Metadata</strong>: ' + meta_nm + '</li>');
                stats.append('<li><strong>Label</strong>: ' + meta_anot + '</li>');
            }
        }
    };

    self.hover.onmouseout = function (e) {
        self.hoverctx.clearRect(0, 0, parseInt(width), parseInt(height));
    };

    //allow data update
    self.update = function (data, nms) {
        self.data = data;
        self.nms = nms;
    };
    self.setbatchmode = function (mode, start_y_pos) {
        self.batchmode = mode;
        self.start_y_pos = start_y_pos;
    };
    self.setdragmode = function (mode) {
        self.dragmode = mode;
    };
    self.set_metaanot = function (metaanot) {
        self.metaanot = metaanot;
    };

    self.totwidth = function () {
        return opts.totwidth;
    };
    self.dotwidth = function () {
        return opts.dotwidth;
    };
    return self;
}
;

function updateCellColor(color, id) {

    $("#" + id).css("background-color", color.replace('0x', '#'));
    var inx = id.split("_")[1]
    if (color === "#ffffff") {
        document.getElementById(id).innerHTML = "";
    } else {
        document.getElementById(id).innerHTML = "  P" + inx + "  ";
    }
}

function openModuleColorPicker(ev) {
    current_module = ev.id
    current_module_color = $("#" + current_module).css("backgroundColor");
    var a = current_module_color.split("(")[1].split(")")[0];
    a = a.split(",");
    current_module_color = "#" + convertToHex(a)
    //$("#customModule").spectrum("set", current_module_color);
    $('#modulecolordlg').dialog('open');
}

function convertToHex(rgb) {
    return hex(rgb[0]) + hex(rgb[1]) + hex(rgb[2]);
}

function hex(c) {
    var s = "0123456789abcdef";
    var i = parseInt(c);
    if (i == 0 || isNaN(c))
        return "00";
    i = Math.round(Math.min(Math.max(0, i), 255));
    return s.charAt((i - i % 16) / 16) + s.charAt(i % 16);
}

function getAllIndexes(arr, val) {
    var indexes = [], i = -1;
    while ((i = arr.indexOf(val, i + 1)) != -1) {
        indexes.push(i);
    }
    return indexes;
}

function clearAnnot() {
    annot_arr = []
    annot_arr_length = []
    annot_arr2 = []
    annot_arr3 = [];
    annot_inx_arr = []
    sel_inx = []
    annot_colors = []
    main_map.render();
    smpl_map.render();
}

function splitIndex(value, index) {
    return value.substring(0, index) + "," + value.substring(index);
}

function setHeatmapEnrichOpts(org, type) {
    var qOpts = $('#enrichdb');
    qOpts.empty();
    if (type === "mummichog") {
        qOpts.append(
                '<option value="kegg">KEGG</option>' +
                '<option value="reactome">Reactome</option>' +
                '<option value="biocyc">Biocyc</option>' +
                '<option value="lipid_class_mset">Lipid</option>' +
                '<option value="nolipid_class_mset">Non-Lipid</option>' +
                '<option value="mfn">MFN</option>'
                );
    } else if (type === "pathway") {
        if (org === "hsa" || org === "mmu") {
            qOpts.append(
                    '<option value="keggc">KEGG</option>' +
                    '<option value="smpdb">SMPDB</option>'
                    );
        } else {
            qOpts.append(
                    '<option value="keggc">KEGG</opt>'
                    );
        }

        if (libOpt.includes("smpdb")) {
            $('#enrichdb').val("smpdb");
        }
        $("#enrichdb").prop("disabled", true);
    }
}

function setEnrichOpts(org) {
    //console.log(org)
    var qOpts = $('#enrichdb');
    qOpts.empty();
    if (org === "hsa") {
        qOpts.append(
                '<option value="hsa_kegg">KEGG</option>' + '<option value="hsa_mfn">Recon2</option>' +
                '<option value="hsa_biocyc">BioCyc</option>'
                )
    } else if (["mmu", "dme", "sce"].indexOf(org) !== -1) {
        qOpts.append(
                '<option value="' + org + '_mfn">BioCyc</option>' +
                '<option value="' + org + '_kegg">KEGG</option>');
    } else if (["rno", "bta", "gga", "cel", "osa", "ath", "smm", "pfa", "tbr", "eco", "bsu", "ppu", "sau", "tma", "syf", "mlo"].indexOf(org) !== -1) {
        qOpts.append('<option value="' + org + '_kegg">KEGG</option>'
                );
    } else if (org === "dre") {
        qOpts.append(
                '<option value="' + org + '_kegg">KEGG</option>' +
                '<option value="' + org + '_mtf">MTF</option>');
    }


}

function setupTheme() {
    if (heat.updateHeatmapColors === null || heat.updateHeatmapColors === undefined) {
        return;
    }
    var light = $(parent.window.document).find("#formHidden\\:selectedTheme").val();

    if (light === "light") {
        textColor = 'rgba(0, 0, 0, 1)';
        bgColor = 'rgba(255, 255, 255, 1)';
    } else {
        textColor = 'rgba(255, 255, 255, 1)';
        bgColor = 'rgba(0, 0, 0, 1)';

    }
    var col = $('#color').val();
    heat.updateHeatmapColors(col);
}

