

//if type = "default", network_default.png
function sendImageToServer(dataURL, name, format = "png", callback) {
    $.ajax({
        dataType: "html",
        type: "POST",
        url: '/MetaboAnalyst/faces/AjaxCall',
        data: {function: 'decodeImageData', data: dataURL, name: name, format: format},
        async: false,
        cache: false,
        error: function () {
            console.log("Image saving error!");

        },
        success: function () {
            console.log("Image saved");
            if (typeof callback === "function") {
                callback();
            }
        }
    });
}


function sendJsonToServer(jsonData, type, generateReport) {
    $.ajax({
        dataType: "html",
        type: "POST",
        url: '/MetaboAnalyst/faces/AjaxCall',
        data: {function: 'receiveJsonData', data: jsonData, name: type},
        async: false,
        cache: false,
        error: function () {
            console.log("Error: JSON saving");
        },
        success: function () {
            console.log("Success: JSON saving");
            //console.log(generateReport)
            if ($(parent.window.document).find("#sidebar-form\\:m_report")) {
                if (generateReport) {
                    parent.generateReportFromJS();
                    parent.PF('growlWidget').show([{severity: "info", summary: "INFO", detail: "Report successfully updated! Now, generating report..."}]);

                } else {
                    parent.PF('growlWidget').show([{severity: "info", summary: "INFO", detail: "Report successfully updated!"}]);
                }
            } else {
                if (generateReport) {
                    generateReportFromJS();
                    PF('growlWidget').show([{severity: "info", summary: "INFO", detail: "Report successfully updated! Now, generating report..."}]);
                } else {
                    PF('growlWidget').show([{severity: "info", summary: "INFO", detail: "Report successfully updated!"}]);
                }
            }
        }
    });
}

// dynamically change CSS for html (easyui)
function changeIframeCSS(cssFile, cssLinkIndex) {

    var oldlink = document.getElementsByTagName("link").item(cssLinkIndex);
    if (!oldlink) {
        console.error("No link element found at index " + cssLinkIndex);
        return;
    }
    var newlink = document.createElement("link");
    newlink.setAttribute("rel", "stylesheet");
    newlink.setAttribute("type", "text/css");
    newlink.setAttribute("href", cssFile);

    document.getElementsByTagName("head").item(cssLinkIndex).replaceChild(newlink, oldlink);
}

//only for easyui iframe
function initThemeSwitch() {
    //var element2 = parent.window.document.getElementById("#menubuttonFormId\\:themelnk")
    var element = $(parent.window.document).find("#menubuttonFormId\\:themelnk");
    element.bind('click keypress', function (event) {
        updateTheme();
    });
}


function switch2LightTheme() {
    changeIframeCSS("/MetaboAnalyst/resources/css/easyui.light.css", 0);
}

function switch2DarkTheme() {
    changeIframeCSS("/MetaboAnalyst/resources/css/easyui.dark.css", 0);
}

function switch2DimTheme() {
    changeIframeCSS("/MetaboAnalyst/resources/css/easyui.dim.css", 0);
}

function changeIframeCSS(cssFile, cssLinkIndex) {

    var oldlink = document.getElementsByTagName("link").item(cssLinkIndex);

    var newlink = document.createElement("link");
    newlink.setAttribute("rel", "stylesheet");
    newlink.setAttribute("type", "text/css");
    newlink.setAttribute("href", cssFile);

    document.getElementsByTagName("head").item(cssLinkIndex).replaceChild(newlink, oldlink);
}

function updateTheme() {
    var light = $(parent.window.document).find("#formHidden\\:selectedTheme").val();
    console.log(light);
    if (light === "light") {
        changeIframeCSS("../../resources/css/easyui.light.css", 0);
    } else if (light === "dim"){
        changeIframeCSS("../../resources/css/easyui.dim.css", 0);
    }else {
        changeIframeCSS("../../resources/css/easyui.dark.css", 0);
    }
}

function copyShareLink() {
    var text = document.getElementById("formShareLink:shareLink").innerHTML;
    var input = document.createElement('textarea');
    input.innerHTML = text;
    document.body.appendChild(input);
    input.select();
    var result = document.execCommand('copy');
    document.body.removeChild(input);
    //console.log("copyShareLink called====");
    return result;
}

function rebindIframe() {
    if (document.getElementById('imgForm')) {
        if (document.getElementById('imgForm').querySelector('[id$="themeHt"]')) {
            document.getElementById('imgForm').querySelector('[id$="themeHt"]').value = parent.PF('toggleVar').input[0].checked;
            setupTheme();
        }
    }
    if (document.getElementById('toggleTmReport')) {
        document.getElementById('toggleTmReport').click();
    }

    if (document.getElementById('iframeId')) {
        var iframeDocument = document.getElementById('iframeId')
        if (iframeDocument.contentDocument.getElementById('themeHt')) {
            iframeDocument.contentDocument.getElementById('themeHt').value = parent.PF('toggleVar').input[0].checked;
            iframeDocument.contentDocument.getElementById('toggleTm').click();
        }
    } else if (document.getElementById('themeHt')) {
        //document.getElementById('themeHt').value = parent.PF('toggleVar').input[0].checked;
        setupTheme();
    }

}


