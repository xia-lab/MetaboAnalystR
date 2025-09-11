/*
 * Java script for detecting user interactions and
 * making asynchronous/synchronous  ajax calls to the backend server.
 * (Adapted from AVIS script by Berger SI, et al.)
 *
 * Author: Jeff Xia
 */


//  Initialize a bunch of variables
var zoom = 100;
var xpos = 50;
var ypos = 50;

var imgWidth = 100;
var imgHeight = 100;
var baseWidth = 100;
var baseHeight = 100;

var xd = 0;
var yd = 0;

var frameWidth = 10;
var frameHeight = 10;

var startZoomer = 0;
var paView = "pathway"; //pathway or pathinteg
var pannerX = 0;
var pannerY = 0;

var imloaded = false;
var fgimloaded = false;

var fgbaseWidth = 10;
var fgbaseHeight = 10;
var fgbaseZoom = 100;
var fgxoffset = 0;
var fgyoffset = 0;
var drag = 0;

var clickMapLoaded = false;

var LCzoompc = 0;
var LCpanXpc = 0;
var LCpanYpc = 0;

var keggPathLnk = "";
var pathImgURL = null;
var zoomImgURL = null;
var redraw = 0;
var pathPrefix = "";

var isClick = false;

var leftImgWidth = 10;
var leftImgHeight = 10;
var leftImgURL = null;

var loadWidth = 10;
var loadHeight = 10;

var rectArray = new Array();
var circleArray = new Array();

var paneWidth = 10;
var paneHeight = 10;

var vizMode = "pathwayviewer";
//first check if in iframe, to fix domain URL
//the path on pulbic server should be 
//https://www.metaboanalyst.ca/faces/
//not http://www.metaboanalyst.ca/MetaboAnalyst/faces/

function initPathwayViewer() {
    $.ajax({
        beforeSend: function () {
            $.messager.progress({
                text: 'Retrieving information .....'
            });
        },
        dataType: "html",
        type: "GET",
        url: '/MetaboAnalyst/faces/AjaxCall',
        data: 'function=getImageMap',
        async: false,
        cache: false,
        success: function (result) {
            console.log(result);
            var res = result.split("||");
            paView = res[0];
            setLeftImageMap(res[1]);
            $.messager.progress('close');

            if (document.getElementById('viewer')) {
                resized();
                resize(100, 50, 50);
            }
            //perform default enrichment analysis
            $("#spinner").fadeOut("slow");
        },
        error: function (result) {
            //alert(result);
            $.messager.alert('Error', 'Failed to process the request! Try to clear Browser History and perform analysis again.', 'error');
            $.messager.progress('close');
        }
    });
}

//For SMPDB, exclude the call to setup right panel
function initSMPDBPathwayViewer() {
    //inject SMPDB specific action
    if (window.location !== window.parent.location) {
        vizMode = "network";
    } else {
        vizMode = "network-full";
        $('#networkspace').css("height", 801);
        $('#networkspace').css("width", 921);
        $('#networkview').css("width", 860);
        $('#networkview').css("height", "88%");
        $('#sigma_mouse_1').css("width", "0px");
        $('#ctrlpanel').css("top", 801);
        $('#ctrlpanel').css("left", 300);
        if (sigInst !== undefined) {
            sigInst.refresh();
        }
    }

    initPathwayViewer();
}

//Main function for positioning the image in the viewer
function resize(zoomfactor, xpos2, ypos2) {

    xpos = !isNaN(parseFloat(xpos2)) ? parseFloat(xpos2) : xpos;  //Validate the xpos
    xpos = xpos < 0 ? 0 : xpos > 100 ? 100 : xpos;
    ypos = !isNaN(parseFloat(ypos2)) ? parseFloat(ypos2) : ypos;  //Validate the ypos
    ypos = ypos < 0 ? 0 : ypos > 100 ? 100 : ypos;

    zoomfactor = parseFloat(zoomfactor);   //validate the zoomfactor

    if (!parseFloat(zoomfactor)) {
        zoomfactor = zoom;
    }
    if (zoomfactor < 100) {
        zoomfactor = 100;
    }
    if (zoomfactor === 100) {
        xpos = 50;
        ypos = 50;
    }
    zoom = zoomfactor;
    imgWidth = parseInt(baseWidth * zoom / 100);
    imgHeight = parseInt(baseHeight * zoom / 100);
    if (imgWidth < paneWidth || imgHeight < paneHeight) {      //Make sure you don't zoom out more than you can...
        zoom = zoom * Math.max(paneWidth / imgWidth, paneHeight / imgHeight);
        zoomfactor = zoom;
        imgWidth = parseInt(baseWidth * zoomfactor / 100);
        imgHeight = parseInt(baseHeight * zoomfactor / 100);
    }
    var fgimgWidth = parseInt(fgbaseWidth * 100 / fgbaseZoom * zoom / 100);   //calculate the size of the overlay image
    var fgimgHeight = parseInt(fgbaseHeight * 100 / fgbaseZoom * zoom / 100);

    var pw = paneWidth;
    var ph = paneHeight;

    //calculate the limits on the positioning of the image
    var mnxp = pw - imgWidth < 0 ? pw - imgWidth : 0;
    var mxxp = pw - imgWidth < 0 ? 0 : pw - imgWidth;
    var mnyp = ph - imgHeight < 0 ? ph - imgHeight : 0;
    var mxyp = ph - imgHeight < 0 ? 0 : ph - imgHeight;

    var l1 = parseInt((xpos * (mxxp - mnxp) / 100) + mnxp);  //calculate the position of the image
    var l2 = parseInt((ypos * (mxyp - mnyp) / 100) + mnyp);
    //calculate the position of the overlay image
    var l3 = parseInt(((xpos * (mxxp - mnxp) / 100) + mnxp + ((imgWidth - fgimgWidth) * fgxoffset / 100)));
    var l4 = parseInt(((ypos * (mxyp - mnyp) / 100) + mnyp + ((imgHeight - fgimgHeight) * fgyoffset / 100)));

    document.getElementById('imagepanel').style.left = l1 + "px";  //set the background image panel position
    document.getElementById('imagepanel').style.top = l2 + "px";

    document.getElementById('fgimagepanel').style.left = l3 + "px";  //set the forground image panel position
    document.getElementById('fgimagepanel').style.top = l4 + "px";

    document.getElementById('imagepanel').style.width = imgWidth + "px";   //set the background image panel size
    document.getElementById('imagepanel').style.height = imgHeight + "px";

    if (document.getElementById('theImage')) {                         //set the background image size
        document.getElementById('theImage').width = imgWidth;
        document.getElementById('theImage').height = imgHeight;
    }

    document.getElementById('fgimagepanel').style.width = parseInt(fgimgWidth) + "px";   //set forground image panel size
    document.getElementById('fgimagepanel').style.height = parseInt(fgimgHeight) + "px";

    if (document.getElementById('thefgImage')) {  //set forground image size
        document.getElementById('thefgImage').width = paneWidth;
        document.getElementById('thefgImage').height = paneHeight;
    }

    document.getElementById('top').style.left = l1 + "px";                             //bosition the overlay screen
    document.getElementById('top').style.top = l2 + "px";
    document.getElementById('top').style.width = imgWidth + "px";
    document.getElementById('top').style.height = imgHeight + "px";

    //See if the view has sufficiently changed
    var centDrift = (((100 - xpos - fgxoffset) / (10000 / fgbaseZoom)) * ((100 - xpos - fgxoffset) / (10000 / fgbaseZoom))) + (((100 - ypos - fgyoffset) / (10000 / fgbaseZoom)) * ((100 - ypos - fgyoffset) / (10000 / fgbaseZoom)));
    var zoomDrift = Math.abs(fgbaseZoom - (imgHeight / paneHeight * 100));

    if ((startZoomer === 0 && pannerX === 0 && pannerY === 0 && drag === 0 && imloaded &&
            (centDrift > .01 || zoomDrift > 1)) || redraw === 1) {
        redraw = 0;
        getfgSession(loadWidth, loadHeight, imgHeight / paneHeight * 100, 100 - xpos, 100 - ypos);  // if view has sufficiently changed, call for a rerendering of foreground
    }
    document.getElementById('imagepanel').style.visibility = 'hidden';
    document.getElementById('imagepanel').style.visibility = 'visible';
    document.getElementById('top').style.visibility = 'hidden';
    document.getElementById('top').style.visibility = 'visible';
}


function setLeftImageMap(jscode) {
    if (jscode.substr(0, 10) === "leftImgURL") {
        circleArray = new Array();
        eval(jscode);
        document.getElementById('overview').innerHTML = '<IMG border="0" id="theLeftImage" width="' + leftImgWidth + '" height="' + leftImgHeight + '"src="' + leftImgURL + '" />';
    } else {
        $.messager.alert('Error', "Your session time is out! \n Please log out and try again");
    }
}

function renderImage(pathname) {

    if (vizMode === "network" || vizMode === "network-full") {
        loadNetwork(pathname);
        return;
    }

    prepareInit(pathname);

    $.ajax({
        beforeSend: function () {
            $.messager.progress({
                text: 'Retrieving information .....'
            });
        },
        dataType: "html",
        type: "GET",
        url: '/MetaboAnalyst/faces/AjaxCall',
        data: 'function=plotPathway' +
                '&pathname=' + pathname +
                "&width=" + loadWidth +
                "&height=" + loadHeight +
                "&analType=metpa" +
                "&timestamp=" + new Date().getTime(),
        async: false,
        cache: false,
        timeout: 8000,
        success: function (result) {
            loadImage(result);
            if($.messager){
            $.messager.progress('close');
        }
        },
        error: function () {
            $.messager.alert('Error', 'Failed to process the request! Try to clear Browser History and perform analysis again.', 'error');
            $.messager.progress('close');
        }
    });
}

function prepareInit(pathname) {
    //set to default zoom and pan
    loadWidth = paneWidth;
    loadHeight = paneHeight;
    baseWidth = paneWidth;
    baseHeight = paneHeight;
    redraw = 0;
    zoom = 100;

    imloaded = false;
    fgimloaded = false;
    pathImgURL = null;
    zoomImgURL = null;
    clickMapLoaded = false;

    document.getElementById('top').style.left = 0 + "px";                             //bosition the overlay screen
    document.getElementById('top').style.top = 0 + "px";
    document.getElementById('top').style.height = paneHeight;
    document.getElementById('top').style.width = paneWidth;

    document.getElementById('top').innerHTML = "<br/>Please wait ....";
    document.getElementById('top').style.opacity = 1;
    if (document.getElementById('top').filters)
    {
        document.getElementById('top').filters.alpha.opacity = 100;
    }
    document.getElementById('top').innerHTML = "<br/>Rendering " + pathname + " pathway....";
    document.getElementById('top').style.visibility = 'hidden';
    document.getElementById('top').style.visibility = 'visible';

    document.getElementById('imagepanel').style.height = paneHeight;
    document.getElementById('fgimagepanel').style.opacity = 0;
    if (document.getElementById('fgimagepanel').filters) {
        document.getElementById('fgimagepanel').filters.alpha.opacity = 0;
    }
}

function loadImage(response) {
    if (response.substr(0, 10) === "pathImgURL") {
        baseWidth = paneWidth;
        baseHeight = paneHeight;
        imgWidth = baseWidth;
        imgHeight = baseHeight;
        rectArray = new Array();
        clickMapLoaded = true;
        eval(response);
        loadImage2();
    } else {
        $.messager.alert('Error', 'Failed to process the request! Try to clear Browser History and perform analysis again.', 'error');
    }
}

// adapted from original function sessionFetched2()
function loadImage2() {
    document.getElementById('title').innerHTML = keggPathLnk;
    //document.getElementById('title').innerHTML ='<a href="javascript:void(0);" onclick="window.open(\'http://www.genome.jp/kegg-bin/show_pathway?ath00030\',\'KEGG\');">Pentose phosphate pathway</a>';
    document.getElementById('top').innerHTML = "Loading new image...";
    //place the background image into an HTML panel and start it loading
    document.getElementById('imagepanel').innerHTML = '<IMG border="0" id="theImage" width="' + imgWidth + '" height="' + imgHeight + '"src="' + pathImgURL + '" />';
    zoom = 100 * parseFloat(document.getElementById('imagepanel').style.height) / baseHeight;  //save the zoom

    document.getElementById('top').innerHTML = "";
    document.getElementById('top').style.opacity = 0;
    document.getElementById('fgimagepanel').style.opacity = 0;

    //make sure nothing is blocking the view
    if (document.getElementById('top').filters) {
        document.getElementById('top').filters.alpha.opacity = 0;
    }

    imloaded = true;
    //console.log("inside loadImage2 " + rectArray.length);
    resize(100, 50, 50);
}

//load a new foreground image
function getfgSession(loadWidth, loadHeight, zoompc, panXpc, panYpc) {
    if (clickMapLoaded) {  //only do this if the click map has been loaded
        LCzoompc = zoompc;
        LCpanXpc = panXpc;
        LCpanYpc = panYpc;
        fgimloaded = false;

        $.ajax({
            beforeSend: function () {
                $.messager.progress({
                    text: 'Retrieving information .....'
                });
            },
            dataType: "html",
            type: "GET",
            url: '/MetaboAnalyst/faces/AjaxCall',
            data: 'function=redraw' +
                    "&zoompc=" + zoompc +
                    "&panXpc=" + panXpc +
                    "&panYpc=" + panYpc +
                    "&width=" + loadWidth +
                    "&height=" + loadHeight +
                    "&isMetPA=" + paView +
                    "&analType=metpa" +
                    "&timestamp=" + new Date().getTime(),
            async: false,
            cache: false,
            timeout: 8000,
            success: function (result) {
                fgsessionFetched(result, zoompc, panXpc, panYpc);
                $.messager.progress('close');
            },
            error: function () {
                $.messager.alert('Error', "No pathway was detected!");
                $.messager.progress('close');
            }
        });
    } else {
        $.messager.alert('Error', "No pathway was detected!");
    }
}

function fgsessionFetched(resTxt, zoompc, panXpc, panYpc) {

    if (resTxt.substr(0, 10) === "zoomImgURL") {
        eval(resTxt);
        if (zoomImgURL !== "NA") {
            fgbaseWidth = paneWidth;
            fgbaseHeight = paneHeight;
            fgbaseZoom = zoompc;
            fgxoffset = panXpc;
            fgyoffset = panYpc;
            fgsessionFetched2();
        } else {
            $.messager.alert('Error', "Failed to create a zoom-in image. If you are the Sys Admin, please make sure that the convert command is available in the PATH.");
        }
    } else {
        $.messager.alert('Error', 'Failed to process the request! Try to clear Browser History and perform analysis again.', 'error');
    }
}

function fgsessionFetched2() {
    var centDrift = (((100 - xpos - fgxoffset) / (10000 / fgbaseZoom)) * ((100 - xpos - fgxoffset) / (10000 / fgbaseZoom))) + (((100 - ypos - fgyoffset) / (10000 / fgbaseZoom)) * ((100 - ypos - fgyoffset) / (10000 / fgbaseZoom)));
    var zoomDrift = Math.abs(fgbaseZoom - (imgHeight / paneHeight * 100));
    if (centDrift <= .01 && zoomDrift <= 1) {
        fgimloaded = true;
        //make the current forground disappear
        document.getElementById('fgimagepanel').style.opacity = 0;
        if (document.getElementById('fgimagepanel').filters) {
            document.getElementById('fgimagepanel').filters.alpha.opacity = 0;
        }  //make the current forground disappear
        //load the new forground image, and make it visible when its done loading
        document.getElementById('fgimagepanel').innerHTML = '<IMG border="0" id="thefgImage" src="' + zoomImgURL + '"onload="javascript:imageloaded();"/>';
    }
}

function imageloaded() {  // the foreground image has loaded, make it visible
    document.getElementById('fgimagepanel').style.opacity = 1;  //make it visible
    if (document.getElementById('fgimagepanel').filters)
    {
        document.getElementById('fgimagepanel').filters.alpha.opacity = 100;
    }
    resize(zoom, xpos, ypos);  //position the image
}

function Evt(evt) {   //function to wrap mouse event calls to store locations of event
    this.evt = evt ? evt : window.event;
    this.source = evt.target ? evt.target : evt.srcElement;
    this.x = evt.pageX ? evt.pageX : evt.clientX;
    this.y = evt.pageY ? evt.pageY : evt.clientY;
}

Evt.prototype.consume = function () {  //prevents propagation of mouse events to lower layers
    if (this.evt.stopPropagation) {
        this.evt.stopPropagation();
        this.evt.preventDefault();
    } else if (this.evt.cancelBubble) {
        this.evt.cancelBubble = true;
        this.evt.returnValue = false;
    }
};

Evt.addEventListener = function (target, type, func, bubbles) { //allows event listener to be added to document, used to switch into drag mode
    if (document.addEventListener) {
        target.addEventListener(type, func, bubbles);
    } else if (document.attachEvent) {
        target.attachEvent("on" + type, func, bubbles);
    } else {
        target["on" + type] = func;
    }
};

Evt.removeEventListener = function (target, type, func, bubbles) {  //removes an event listener, allows switching out of drag mode
    if (document.removeEventListener) {
        target.removeEventListener(type, func, bubbles);
    } else if (document.detachEvent) {
        target.detachEvent("on" + type, func, bubbles);
    } else {
        target["on" + type] = null;
    }
};

function dragPress2(evt) {   //catches when the mouse is pressed on the map
    var clickAt = getClickAt2(evt);   //store the locations of the mouse down
    var clickatX = clickAt.x;
    var clickatY = clickAt.y;
    xd = evt.x;
    yd = evt.y;
    pannerX = 0;
    nd();   //cancel any active overlib messages

    for (var inx = 0; inx < circleArray.length; inx++) {   //check if this mouse click was on a node (circle method)
        var circle = circleArray[inx];
        if (((clickatX - circle.xc) * (clickatX - circle.xc)) + ((clickatY - circle.yc) * (clickatY - circle.yc)) < circle.r * circle.r) {
            if (vizMode.startsWith("network")) {
                loadNetwork(circle.lb);
            } else {
                renderImage(circle.lb);
            }
        }
    }
}

function panelMouseMove2(evt) {  //catches when the mouse moves over the image, displays the mouseover overlib messages
    var clickAt = getClickAt2(evt);
    clickatX = clickAt.x;
    clickatY = clickAt.y;
    nd(); // cancel any visible overlib messages
    for (var inx = 0; inx < circleArray.length; inx++) {    //check if this mouse was on a node (circle method)
        var circle = circleArray[inx];
        if (((clickatX - circle.xc) * (clickatX - circle.xc)) + ((clickatY - circle.yc) * (clickatY - circle.yc)) < circle.r * circle.r) {
            overlib(circle.lb, WRAP);
        }
    }
}

//pa = 1 for MetPA, 0 for view
function dragPress(evt) {   //catches when the mouse is pressed on the map
    var clickAt = getClickAt(evt);   //store the locations of the mouse down
    var clickatX = clickAt.x;
    var clickatY = clickAt.y;
    evt = new Evt(evt);
    drag = 1;
    xd = evt.x;
    yd = evt.y;
    pannerX = 0;
    Evt.addEventListener(document, "mousemove", dragMove, false);   //puts listeners in place to allow dragging and releasing
    Evt.addEventListener(document, "mouseup", dragRelease, false);
    Evt.addEventListener(document, "mouseout", dragRelease, false);
    nd();   //cancel any active overlib messages
    //console.log("------> dragPress 1: <----" + rectArray.length);
    for (var inx = 0; inx < rectArray.length; inx++) {   //check if this mouse click was on a node (rectangle method)
        var box = rectArray[inx];
        //console.log("------> dragPress 2: <----" + box.x1);
        if (clickatX > box.x1 && clickatX < box.x2 && clickatY > box.y1 && clickatY < box.y2) {
            isClick = true;  //turn on click mode
            if (paView === "pathinteg") {
                var info = "<table>";
                info = info + '<tr><td><font size="1">DB Link:</font></td><td><font size="1">' + box.lnk + "</font></td></tr>";
                if (box.pvalue !== undefined) {
                    info = info + '<tr><td><font size="1">P value:</font></td><td><font size="1">' + box.pvalue + "</font></td></tr>";
                }
                if (box.FDR !== undefined) {
                    info = info + '<tr><td><font size="1">FDR:</font></td><td><font size="1">' + box.FDR + "</font></td></tr>";
                }
                if (box.logFC !== undefined) {
                    info = info + '<tr><td><font size="1">Log FC:</font></td><td><font size="1">' + box.logFC + "</font></td></tr>";
                }
                if (box.topo !== undefined) {
                    info = info + '<tr><td><font size="1">Topology:</font></td><td><font size="1">' + box.topo + "</font></td></tr>";
                }
                //console.log("box.adducts" + box.adducts);
                if (box.adducts !== undefined && box.adducts !== "") {
                    info = info + '<tr><td><font size="1">Adducts:</font></td><td><font size="1">' + box.adducts + "</font></td></tr>";
                }
                if (box.ef !== undefined) {
                    info = info + '<tr><td><font size="1">Effect size:</font></td><td><font size="1">' + box.ef + "</font></td></tr>";
                }
                if (box.combinedP !== undefined) {
                    info = info + '<tr><td><font size="1">Meta Pval:</font></td><td><font size="1">' + box.combinedP + "</font></td></tr>";
                }
                if (box.stat !== undefined) {
                    info = info + '<tr><td><font size="1">Meta Stat:</font></td><td><font size="1">' + box.stat + "</font></td></tr>";
                }
                if (box.vc !== undefined) {
                    info = info + '<tr><td><font size="1">Vote Count:</font></td><td><font size="1">' + box.vc + "</font></td></tr>";
                }
                if (box.rp !== undefined) {
                    info = info + '<tr><td><font size="1">Rank Product:</font></td><td><font size="1">' + box.rp + "</font></td></tr>";
                }
                info = info + "</table>";
                overlib(info, WRAP, STICKY, CAPTION, "Details", CLOSECLICK);
            } else {
                var info = 'Importance : ' + box.impact;
                if (box.pvalue !== "NA") {
                    info = info + '<br/>P value : ' + box.pvalue;
                }

                info = info + '<br/>DB Links : ';

                if (box.kegg !== "NA") {
                    info = info + '<a href="javascript:void(0);" onclick="window.open(\'http://www.genome.jp/dbget-bin/www_bget?' + box.kegg + '\',\'KEGG\');">KEGG</a> '
                }

                if (box.hmdb !== "NA") {
                    info = info + '<a href="javascript:void(0);" onclick="window.open(\'http://www.hmdb.ca/metabolites/' + box.hmdb + '\',\'HMDB\');">HMDB</a>'
                }
                if (box.pvalue !== "NA") {
                    iconURL = '<img src=' + "\'" + pathPrefix + box.icon + ".png\' " + 'width=160 height=160>';
                    overlib(iconURL, WRAP, STICKY, CAPTION, box.lb + "<br/>" + info + " ", CAPBELOW, CLOSECLICK);
                } else {
                    overlib(info, WRAP, STICKY, CAPTION, box.lb + " ", CLOSECLICK);  //display the overlib details
                }
            }
        }
    }
}

function getClickAt(evt) {   //finds where the click is at and corrects for some browser specific differences
    var offsetCorrection = {
        x: 0,
        y: 0
    };
    var dimCorrection = {
        x: 0,
        y: 0
    };    //some corrects for browser specific differences
    if (evt && typeof evt.offsetX !== 'undefined') {
        dimCorrection = {
            x: -3,
            y: -3
        };
        offsetCorrection = {
            x: 0,
            y: 0
        };
    } else {
        dimCorrection = {
            x: -1,
            y: -1
        };
        offsetCorrection = {
            x: -2,
            y: -2
        };
    }
    var ofst = getOffsets(evt);
    evt = new Evt(evt);

    var clickatX;
    var clickatY;

    clickatX = (ofst.x + offsetCorrection.x) * 100 / (parseFloat(document.getElementById('imagepanel').style.width) + dimCorrection.x);
    clickatY = (ofst.y + offsetCorrection.y) * 100 / (parseFloat(document.getElementById('imagepanel').style.height) + dimCorrection.y);
    return {
        x: clickatX,
        y: clickatY
    };
}

function getClickAt2(evt) {   //finds where the click is at and corrects for some browser specific differences
    var offsetCorrection = {
        x: 0,
        y: 0
    };

    //some corrects for browser specific differences
    if (evt && typeof evt.offsetX !== 'undefined') {
        offsetCorrection = {
            x: 0,
            y: 0
        };
    } else {
        offsetCorrection = {
            x: -2,
            y: -2
        };
    }
    var ofst = getOffsets(evt);
    evt = new Evt(evt);

    var clickatX;
    var clickatY;

    clickatX = ofst.x + offsetCorrection.x;
    clickatY = ofst.y + offsetCorrection.y;
    return {
        x: clickatX,
        y: clickatY
    };
}

function panelMouseMove(evt) {  //catches when the mouse moves over the image, displays the mouseover overlib messages
    var clickAt = getClickAt(evt);
    clickatX = clickAt.x;
    clickatY = clickAt.y;

    if (!isClick) {   //only happens if not in click mode
        nd(); // cancel any visible overlib messages
        //check if this mouse was on a node (rectangle method)
        for (var inx = 0; inx < rectArray.length; inx++) {
            var box = rectArray[inx];
            //alert("inside rectloop");
            if (clickatX > box.x1 && clickatX < box.x2 && clickatY > box.y1 && clickatY < box.y2) {
                //  alert("display labeling ");
                overlib(box.lb, WRAP);
            }
        }
    }
}

function getPageCoords(element) {  //find the position of a div on the page
    var coords = {
        x: 0,
        y: 0
    };
    while (element) {
        coords.x += element.offsetLeft;
        coords.y += element.offsetTop;
        element = element.offsetParent;
    }
    return coords;
}

function getOffsets(evt) {  // finds where a mouse event occurs relative to the page
    if (evt) {
        if (typeof evt.offsetX != 'undefined')
            return {
                x: evt.offsetX,
                y: evt.offsetY
            }
        else if (evt.target) {
            var element;
            if (window.opera)
                element = evt.target;
            else
                element = evt.target.nodeType == 1 ? evt.target : evt.target.parentNode;
            var eventCoords = {
                x: evt.clientX + window.pageXOffset,
                y: evt.clientY + window.pageYOffset
            };
            var elCoords = getPageCoords(element);
            return {
                x: eventCoords.x - elCoords.x,
                y: eventCoords.y - elCoords.y
            };
        }
    }
}

function dragMove(evt) {  //called when the mouse is down on the image and it is being dragged...

    evt = new Evt(evt);
    var l1 = parseInt(document.getElementById('imagepanel').style.left) + evt.x - xd;  //find the change in panel x position
    var l2 = parseInt(document.getElementById('imagepanel').style.top) + evt.y - yd;
    var pw = paneWidth;
    var ph = paneHeight;

    var mnxp = pw - imgWidth < 0 ? pw - imgWidth : 0;    //calculate min  and max x positions
    var mxxp = pw - imgWidth < 0 ? 0 : pw - imgWidth;

    l1 = l1 < mnxp ? mnxp : (l1 > mxxp ? mxxp : l1);  //if exceding the boundaried, set position to the boundary

    var mnyp = ph - imgHeight < 0 ? ph - imgHeight : 0;   //repeat with the y position...
    var mxyp = ph - imgHeight < 0 ? 0 : ph - imgHeight;

    l2 = l2 < mnyp ? mnyp : (l2 > mxyp ? mxyp : l2);   //if outside the y boundary... set to the y boundary

    if (drag === 1) {   //if in drag mode....
        xpos = (mxxp - mnxp) !== 0 ? 100 * (l1 - mnxp) / (mxxp - mnxp) : 0;  //recalculate the x position and y position
        ypos = (mxyp - mnyp) !== 0 ? 100 * (l2 - mnyp) / (mxyp - mnyp) : 0;
        resize(zoom, xpos, ypos);  //move the image
    }
    if (document.selection)
        document.selection.empty();  //prevent selecting the image and makign it blue in some browsers
    else if (window.getSelection)
        window.getSelection().removeAllRanges();

    xd = evt.x;
    yd = evt.y;
    evt.consume();  //dont propagate the event more
}

function dragRelease(evt) {  //the mouse has been let go, so the drag can be terminated and event handlers removed
    evt = new Evt(evt);
    drag = 0;
    resize(zoom, xpos, ypos);
    Evt.removeEventListener(document, "mousemove", dragMove, false);
    Evt.removeEventListener(document, "mouseup", dragRelease, false);
    Evt.removeEventListener(document, "mouseout", dragRelease, false);

    if (document.selection)   //prevent selection of the image and it becoming blue in some browsers
        document.selection.empty();
    else if (window.getSelection)
        window.getSelection().removeAllRanges();
    evt.consume();
}

//keep calling until zoomer is stopped...  keeps changing the zoom and redrawing the image
function zoomer() {
    if (startZoomer == 0) {
        startZoomer = 0;        //make sure its no longer zooming
        redraw = 1;
        resize(zoom, xpos, ypos);   //so rerender
        return(false);
    } else {
        var finalzoom = zoom + (startZoomer);
        if (finalzoom > 500) { //max five times
            finalzoom = 500;
        }
        resize(finalzoom, xpos, ypos);   //increase the zoom
        startZoomer = startZoomer > 0 ? startZoomer + 1 : startZoomer - 1;  //accelerate the zooming
        startZoomer = startZoomer > 5 ? 5 : startZoomer < -5 ? -5 : startZoomer; //max out the zoom speed
        setTimeout("zoomer()", 50); //prepare for next iteration
    }
}

function zoomInit() {
    document.getElementById('fgimagepanel').style.opacity = 0;
    if (document.getElementById('fgimagepanel').filters) {
        document.getElementById('fgimagepanel').filters.alpha.opacity = 0;
    }
    redraw = 0;
    zoomer();
}

function scrollto2(xp2, yp2, steps) {
    //stop condition
    if (steps <= 1 || pannerX == 0) {
        pannerX = 0;  //make sure both conditions are met
        redraw = 1;
        resize(zoom, xp2, yp2); //redraw the image
        return(false);
    } else {
        steps = steps - 1;
        var xp3 = (xpos * steps + xp2) / (steps + 1);   //find position after this step
        var yp3 = (ypos * steps + yp2) / (steps + 1);
        resize(zoom, xp3, yp3);                    //reposition the image
        setTimeout("scrollto2(" + xp2 + ", " + yp2 + ", " + steps + ")", 50);  // prepare for next iteration
    }
}

// change the zoom over several steps
function zoomto2(zoom2, steps) {

    if (steps <= 1 || startZoomer == 0) {
        startZoomer = 0;
        resize(zoom2, xpos, ypos);   //set the zoom and rerender
        return(false);
    } else {
        steps = steps - 1;
        var zoom3 = (zoom * steps + zoom2) / (steps + 1);  //calculate new zoom
        resize(zoom3, xpos, ypos);        //adjust the zoom of the image
        setTimeout("zoomto2(" + zoom2 + ", " + steps + ")", 50);  //call next iteration in half a second
    }
}

//function to pan image while button is pressed
function panner() {
    if ((pannerX == 0) && (pannerY == 0)) {
        redraw = 1;
        resize(zoom, xpos, ypos);
        return(false);
    } else {
        resize(zoom, xpos - pannerX, ypos - pannerY);  //move the image
        pannerX = pannerX > 0 ? pannerX + 1 : pannerX < 0 ? pannerX - 1 : 0; //accelerate panner
        pannerX = pannerX > 10 ? 10 : pannerX < -10 ? -10 : pannerX;     //max out pan speed
        pannerY = pannerY > 0 ? pannerY + 1 : pannerY < 0 ? pannerY - 1 : 0;
        pannerY = pannerY > 10 ? 10 : pannerY < -10 ? -10 : pannerY;
        setTimeout("panner()", 50);   //call again in half a second
    }
}

function zoomfit() {
    document.getElementById('fgimagepanel').style.opacity = 0;
    if (document.getElementById('fgimagepanel').filters) {
        document.getElementById('fgimagepanel').filters.alpha.opacity = 0;
    }
    redraw = 0;
    startZoomer = 1;
    zoomto2(100, 10);   //set the zoom to 100
}

function resized() {   //Adjust the viewer size to fit the iframe
    if (self.innerWidth) {
        frameWidth = self.innerWidth;
        frameHeight = self.innerHeight;
    } else if (document.documentElement && document.documentElement.clientWidth) {
        frameWidth = document.documentElement.clientWidth;
        frameHeight = document.documentElement.clientHeight;
    } else if (document.body) {
        frameWidth = document.body.clientWidth;
        frameHeight = document.body.clientHeight;
    }

    //paneHeight = parseInt((frameHeight-80) * 0.96);   //Set the size of the viewer
    paneHeight = parseInt(frameHeight - 80);   //Set the size of the viewer
    paneWidth = parseInt(frameWidth * 0.48);

    loadWidth = paneWidth;
    loadHeight = paneHeight;
    baseWidth = paneWidth;
    baseHeight = paneHeight;
    fgimgWidth = paneWidth;
    fgimgHeight = paneHeight;
    document.getElementById('viewer').style.width = "" + parseInt(frameWidth * 0.48) + "px";
    document.getElementById('viewer').style.height = "" + parseInt(frameHeight - 80) + "px";
    document.getElementById('top').style.height = "" + parseInt(frameHeight - 80) + "px";
    document.getElementById('top').style.width = "" + parseInt(frameWidth * 0.48) + "px";
    document.getElementById('controls').style.width = "" + parseInt(frameWidth * 0.48) + "px";
    var cliprect = "rect(0px, " + parseInt(frameWidth * 0.48) + "px, " + parseInt(frameHeight - 80) + "px, 0px)"; //prevent viewer overhang from being visible
    document.getElementById('viewer').style.clip = cliprect;
    document.getElementById('top').style.visibility = 'hidden';
    document.getElementById('top').style.visibility = 'visible';  //force a redraw of the viewer
    redraw = 0;
}

function panset() {  //move the view to specified pan
    var xposN = 100 - parseInt(document.getElementById('xpan').value);
    var yposN = 100 - parseInt(document.getElementById('ypan').value);
    pannerX = 1;
    scrollto2(xposN, yposN, 10);
}

function setimage(imID, value) {  // set the button image
    document.getElementById(imID).src = "../../resources/images/" + value + ".gif";
}

function detectBrowser() {
    var ver = -1; // Return value assumes failure.
    if (navigator.appName == 'Microsoft Internet Explorer')
    {
        var ua = navigator.userAgent;
        var re = new RegExp("MSIE ([0-9]{1,}[\.0-9]{0,})");
        if (re.exec(ua) != null)
            ver = parseFloat(RegExp.$1);

        if (ver < 8.0) {
            $.messager.alert('Error', "Please install a modern web browser.");
        }
    }
}
