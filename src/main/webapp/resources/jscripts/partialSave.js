/* 
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

var ID = function () {
    return Math.random().toString(36).substr(2, 9);
};

function sharingLinkRaw(org, analType, naviString) {
    var linkInfo = getPartialLinkInfo();
    var createdLink = linkInfo[0];
    var jobId = linkInfo[1];

    if (!createdLink.endsWith("/Share?ID=")){
        if (jobId !== '0') {
            PF('shareLinkVar').show();
            populateDialogText(createdLink);
        }
        
        PF('statusDialog').hide();
        return;
    }
    
    var unique_id = ID() + "_" + jobId;
    org = "NA";
    analType = "raw";
    currFileNm = "NA";
    savePartialRaw(currFileNm, analType, naviString, unique_id, function (result) {
        if (result !== "NA") {
            var res = result.split("||");
            var domainurl = res[0];
            var jobid = res[1];
            var link = domainurl + "/MetaboAnalyst/faces/Share?ID=" + unique_id;
            uid = unique_id;
            
            if (jobid !== '0') {
                populateDialogText(link);
                PF('shareLinkVar').show();
            } 
            
        } else {
            //$.messager.alert('', "Error: failed to create the shareable link!", 'error');
        }
        PF('statusDialog').hide();
    });
}

function savePartialRaw(jsonNm, analType, naviString, uid, callBack) {
    $.ajax({
        beforeSend: function () {
            PF('statusDialog').show();
        },
        dataType: "html",
        type: "GET",
        url: '/MetaboAnalyst/faces/AjaxCall',
        data: "function=savePartialRaw" + "&jsonNm=" + jsonNm + "&analType=" + analType + "&naviString=" + naviString + "&uid=" + uid,
        async: false,
        cache: false,
        success: function (result) {
            PF('statusDialog').hide();
            return callBack(result);
        },
        error: function () {
            //$.messager.alert('Error', 'Failed to process the request!', 'error');
            PF('statusDialog').hide();
        }
    });
}

function getPartialLinkInfo() {
    var res = "";
    $.ajax({
        beforeSend: function () {
            PF('statusDialog').show();
        },
        dataType: "html",
        type: "GET",
        url: '/MetaboAnalyst/faces/AjaxCall',
        data: "function=getPartialLinkInfo",
        async: false,
        cache: false,
        success: function (result) {
            res = result.split("||");
        },
        error: function () {
            //$.messager.alert('Error', 'Failed to process the request!', 'error');
            PF('statusDialog').hide();
        }
    });
    PF('statusDialog').hide();
    return res;
}

function populateDialogText(link) {
    var shareLink = $("#shareLink");
    shareLink.empty();
    shareLink.append("Please save this link to resume the process in the future. The link will expire in 14 days.<br/><br/>");
    shareLink.append("Note: it is advised to have only a single window running MetaboAnalyst open at a time.<br/><br/>");
    shareLink.append('<strong><a id="slink" href="' + link + '" target="_blank"></u>' + link + '</a></strong>');
}

function copyShareLink() {
    var copyText = document.getElementById("slink").innerHTML;
    var res = copy(copyText)
    PF('shareLinkVar').hide();
}

function copy(text) {
    var input = document.createElement('textarea');
    input.innerHTML = text;
    document.body.appendChild(input);
    input.select();
    var result = document.execCommand('copy');
    document.body.removeChild(input);
    return result;
}