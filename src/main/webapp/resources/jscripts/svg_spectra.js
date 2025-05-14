/* 
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
var gridSVGCoords = "";
var gridSVGMappings = "";

// select elements

function showTooltip(evt, text, type) {
    var container = document.getElementById('container');
    var rect = container.getBoundingClientRect();
    let tooltip = document.getElementById("tooltip" + type);
    tooltip.innerText = text;
    tooltip.style.display = "inline-block";
    var doc = evt.target;

    tooltip.style.left = evt.clientX + container.scrollLeft - rect.left;
    +"px";
    tooltip.style.top = evt.clientX + container.scrollLeft - rect.left;
    +"px";

}

function initSvgInteractions() {
    var res = $.ajax({
        type: "GET",
        url: '/MetaboAnalyst/faces/AjaxCall?function=getSpecSvgInfo&ignoreMe=' + new Date().getTime(),
        async: false
    }).responseText;
    //console.log("initSvgInteractions res: " + res);
    res = res.split("||");
    var usr_dir = res[0];
    var fileNm = res[1];
    //console.log("initSvgInteractions res: " + res);
    //console.log("initSvgInteractions usr_dir: " + usr_dir);
    //console.log("initSvgInteractions fileNm: " + fileNm);

    var plot_types = Array.from(document.getElementsByName('name'));
    //console.log("initSvgInteractions plot_types: " + plot_types);
    
    $.ajaxSetup({
        async: false
    });
    
 //   console.log("initSvgInteractions <---- running 0");

    $.getJSON(usr_dir + fileNm,
            function (data) {
                var i = 0;// add event listeners
                var j = 0;
                setTimeout(function () {
                    //console.log("initSvgInteractions getJSON <---- running 1");
                    Array.from(document.querySelectorAll('use')).forEach(function (el) {
                      //  console.log("initSvgInteractions getJSON <---- running 2" + el);
                      //  console.log("initSvgInteractions el.id <---- running 2.5" + el.id);
                        if (el.id !== undefined) {
                       //     console.log("initSvgInteractions getJSON <---- running 3");
                            if (el.id.includes("geom_point")) {
                        //        console.log("initSvgInteractions getJSON <---- running 4");
                                el.addEventListener("dblclick", plotXIC);
                                el.addEventListener("touchstart", startHover);
                                el.addEventListener("mouseenter", startHover);
                                el.addEventListener("touchmove", moveHover);
                                el.addEventListener("mouseleave", moveHover);
                        //        console.log("initSvgInteractions getJSON <---- running 5");
                            }
                        }
                    })

                    Array.from(document.querySelectorAll('foreignObject')).forEach(function (el) {
                        el.style.pointerEvents = "none";
                    })


                }, 1000);

              //  console.log("initSvgInteractions getJSON <---- running 6");
                function plotXIC(e) {
                    if (e.target.id.includes("geom_point")) {
                        var id = e.target.id;
                        var match = "\.";
                        var index = parseFloat(id.slice(id.lastIndexOf(match) + match.length)) - 1;
                        var smplName = data.label[index]
                        getSingleXIC(fileNm.replace(".json", ""), smplName);
                    }
                }
//console.log("initSvgInteractions getJSON <---- running 7");
                function startHover(e) {                    
                    var id = e.target.id;
                    var match = "\.";
                    var index = parseFloat(id.slice(id.lastIndexOf(match) + match.length)) - 1;
                    var smplName = data.label[index]
                    //e.target.append($("span.tip-content"))
                    $("span.tip-content").text("Sample: " + smplName);
                    let numlen = (smplName.length + 6)*10;
                    var pos = getElementAbsolutePos(e.target);
                    var dlg = PF('BoxPlotdialog').jqEl;
                    var xPos = e.pageX - dlg.offsetLeft + 20;
                    var yPos = e.pageY - dlg.offsetTop - PF('BoxPlotdialog').content[0].offsetTop - 20;
                    $("span.tip-content").css({"opacity": "0.85", "left": xPos + "px", "top": yPos + "px", "z-index": "99999", "width": numlen, "position": "absolute"});

                    e.target.setAttribute("height", "10.06");
                    e.target.setAttribute("width", "10.06");
                }
//console.log("initSvgInteractions getJSON <---- running 8");
                function moveHover(e) {                    
                    $("span.tip-content").css({ "left":  "-999px", "top": "-999px","opacity": "0"});
                    e.target.setAttribute("height", "8.06");
                    e.target.setAttribute("width", "8.06");
                    //e.target.setAttribute("transform", "scale(1)");
                }

            });



    //var zoomTigerArr = []
    //for (var i = 0; i < svg_arr.length; i++) {
    /*
     setTimeout(function () {
     var arr = ["svgboxplot"]
     lastEmbed = document.getElementById("svgboxplot")
     lastEventListener = function () {
     svgPanZoom("#" + "svgboxplot", {
     zoomEnabled: true,
     controlIconsEnabled: true,
     dblClickZoomEnabled: false
     });
     }
     lastEmbed.addEventListener('load', lastEventListener)
     
     }, 1000);*/
    $.ajaxSetup({
        async: true
    });
}

function createNewEmbed(id, j) {
    var embed = document.getElementById(id)
    //lastEventListener = function () {
    var addBool = true;
    for (var i = 0; i < embed.children.lengt; i++) {
        var el = embed.children[i];
        if (el.id.includes("svg-pan-zoom-controls")) {
            addBool = false;
        }
    }
    if (addBool) {
        window[["zoomTiger"]] = svgPanZoom(embed, {
            zoomEnabled: true,
            controlIconsEnabled: true
        });
    }
    //}
    //embed.addEventListener('load', lastEventListener)
    //console.log(embed)

    return embed;
}

function removeEmbed() {
    svgPanZoom(lastEmbed).destroy()
    // Remove event listener
    lastEmbed.removeEventListener('load', lastEventListener)
    //console.log(lastEmbed)
    // Null last event listener
    //lastEventListener = null;
    //lastEmbed = null
}


function getSingleXIC(featureNumber, sampleName) {

    $.ajax({
        beforeSend: function () {
            PF('statusDialog').show();
        },
        dataType: "html",
        type: "POST",
        url: '/MetaboAnalyst/faces/AjaxCall',
        data: {function: 'loadingSingleXICPlot', featureNumber: featureNumber, sampleName: sampleName},
        async: false,
        success: function (result) {
            PF('statusDialog').hide();
            //PF('EIC').show();
            //PF('BoxPlotSvgdialog').show();
            //console.log("Ready to updateSingleXICs");
            updateSingleXICs();
        }
    });
}

function __getIEVersion() {
    var rv = -1; // Return value assumes failure.
    if (navigator.appName == 'Microsoft Internet Explorer') {
        var ua = navigator.userAgent;
        var re = new RegExp("MSIE ([0-9]{1,}[\.0-9]{0,})");
        if (re.exec(ua) != null)
            rv = parseFloat(RegExp.$1);
    }
    return rv;
}

function __getOperaVersion() {
    var rv = 0; // Default value
    if (window.opera) {
        var sver = window.opera.version();
        rv = parseFloat(sver);
    }
    return rv;
}

var __userAgent = navigator.userAgent;
var __isIE = navigator.appVersion.match(/MSIE/) != null;
var __IEVersion = __getIEVersion();
var __isIENew = __isIE && __IEVersion >= 8;
var __isIEOld = __isIE && !__isIENew;

var __isFireFox = __userAgent.match(/firefox/i) != null;
var __isFireFoxOld = __isFireFox && ((__userAgent.match(/firefox\/2./i) != null) ||
        (__userAgent.match(/firefox\/1./i) != null));
var __isFireFoxNew = __isFireFox && !__isFireFoxOld;

var __isWebKit = navigator.appVersion.match(/WebKit/) != null;
var __isChrome = navigator.appVersion.match(/Chrome/) != null;
var __isOpera = window.opera != null;
var __operaVersion = __getOperaVersion();
var __isOperaOld = __isOpera && (__operaVersion < 10);

function __parseBorderWidth(width) {
    var res = 0;
    if (typeof (width) == "string" && width != null && width != "") {
        var p = width.indexOf("px");
        if (p >= 0) {
            res = parseInt(width.substring(0, p));
        } else {
            //do not know how to calculate other values 
            //(such as 0.5em or 0.1cm) correctly now
            //so just set the width to 1 pixel
            res = 1;
        }
    }
    return res;
}

//returns border width for some element
function __getBorderWidth(element) {
    var res = new Object();
    res.left = 0;
    res.top = 0;
    res.right = 0;
    res.bottom = 0;
    if (window.getComputedStyle) {
        //for Firefox
        var elStyle = window.getComputedStyle(element, null);
        res.left = parseInt(elStyle.borderLeftWidth.slice(0, -2));
        res.top = parseInt(elStyle.borderTopWidth.slice(0, -2));
        res.right = parseInt(elStyle.borderRightWidth.slice(0, -2));
        res.bottom = parseInt(elStyle.borderBottomWidth.slice(0, -2));
    } else {
        //for other browsers
        res.left = __parseBorderWidth(element.style.borderLeftWidth);
        res.top = __parseBorderWidth(element.style.borderTopWidth);
        res.right = __parseBorderWidth(element.style.borderRightWidth);
        res.bottom = __parseBorderWidth(element.style.borderBottomWidth);
    }

    return res;
}

//returns the absolute position of some element within document
function getElementAbsolutePos(element) {
    var res = new Object();
    res.x = 0;
    res.y = 0;
    if (element !== null) {
        if (element.getBoundingClientRect) {
            var viewportElement = document.documentElement;
            var box = element.getBoundingClientRect();
            var scrollLeft = viewportElement.scrollLeft;
            var scrollTop = viewportElement.scrollTop;

            res.x = box.left + scrollLeft;
            res.y = box.top + scrollTop;

        } else { //for old browsers
            res.x = element.offsetLeft;
            res.y = element.offsetTop;

            var parentNode = element.parentNode;
            var borderWidth = null;

            while (offsetParent != null) {
                res.x += offsetParent.offsetLeft;
                res.y += offsetParent.offsetTop;

                var parentTagName =
                        offsetParent.tagName.toLowerCase();

                if ((__isIEOld && parentTagName != "table") ||
                        ((__isFireFoxNew || __isChrome) &&
                                parentTagName == "td")) {
                    borderWidth = kGetBorderWidth
                            (offsetParent);
                    res.x += borderWidth.left;
                    res.y += borderWidth.top;
                }

                if (offsetParent != document.body &&
                        offsetParent != document.documentElement) {
                    res.x -= offsetParent.scrollLeft;
                    res.y -= offsetParent.scrollTop;
                }


                //next lines are necessary to fix the problem 
                //with offsetParent
                if (!__isIE && !__isOperaOld || __isIENew) {
                    while (offsetParent != parentNode &&
                            parentNode !== null) {
                        res.x -= parentNode.scrollLeft;
                        res.y -= parentNode.scrollTop;
                        if (__isFireFoxOld || __isWebKit)
                        {
                            borderWidth =
                                    kGetBorderWidth(parentNode);
                            res.x += borderWidth.left;
                            res.y += borderWidth.top;
                        }
                        parentNode = parentNode.parentNode;
                    }
                }

                parentNode = offsetParent.parentNode;
                offsetParent = offsetParent.offsetParent;
            }
        }
    }
    return res;
}

function appendSVGChild(elementType, target, attributes = {}, text = '') {
    const element = document.createElementNS('http://www.w3.org/2000/svg', elementType);
    Object.entries(attributes).map(a => element.setAttribute(a[0], a[1]));
    if (text) {
        const textNode = document.createTextNode(text);
        element.appendChild(textNode);
    }
    target.appendChild(element);
    return element;
}
;