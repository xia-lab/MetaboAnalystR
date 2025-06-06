var loggedIn = "false";
var metaUploaded = false;
var uploadPermission = "false";

function doSpecLoginAsync() {
    return new Promise((resolve, reject) => {
        dospectloginbutton();  // Call the remoteCommand
        handleLoginResult = function(xhr, status, args) {
            if (args.loggedIn === "true") {
                resolve("true");
            } else {
                resolve("false");
            }
        };
    });
}

document.addEventListener("DOMContentLoaded", function () {
    var uploadWidget = PF('importFile');
    uploadWidget.cancelButton[0].style.display = "none";
    uploadWidget.upload = async function (c) {
        if (loggedIn === "false") {
            PF('statusDialog').show();
        }

        var thisobj = this;

        // Wait for the login check to complete
        loggedIn = await doSpecLoginAsync();
        console.log(loggedIn + "================123");

        if (loggedIn !== "true") {
            return;
        } else {
            PF('statusDialog').hide();
            var uploadPermission = checkUploadPermission();
            
            if (uploadPermission === "false") {
                PF('uploadStatusDialog').show();
                setTimeout(refresh, 5000);
            } else {
                if (!uploadMeta()) {
                    uploadWidget.enableButton(uploadWidget.uploadButton);
                    return;
                }

                if (thisobj.files[0] === undefined || thisobj.files[0] === null) {
                    metaUploaded = false;
                    uploadWidget.files = tempArr;
                }

                if (thisobj.files[0].name.includes(".txt")) {
                    metaUploaded = true;
                } else {
                    PF('importFile').files = tempArr;
                }
                rc([{ name: 'size', value: uploadWidget.files.length }]);
                PrimeFaces.widget.FileUpload.prototype.upload.call(thisobj);
            }
        }
    };

    function swapElement(array, indexA, indexB) {
        var tmp = array[indexA];
        array[indexA] = array[indexB];
        array[indexB] = tmp;
    }
});

function handleLoginResult(xhr, status, args) {
    // This function will be overridden in doSpecLoginAsync
}


function sanityCheckMeta() {
    if (metaUploaded) {
        getMetaIntegrity();
        metaUploaded = false;
    }
}

function checkUploadPermission() {
    res = "false";
    $.ajax({
        beforeSend: function () {

        },
        dataType: "html",
        type: "GET",
        url: '/MetaboAnalyst/faces/AjaxCall',
        data: "function=checkUploadPermission",
        async: false,
        cache: false,
        success: function (result) {
            res = result;
        },
        error: function () {

        }
    });
    return(res);
}

function doSpecLogin() {
    var res = -100;
    $.ajax({
        beforeSend: function () {

        },
        dataType: "html",
        type: "GET",
        url: '/MetaboAnalyst/faces/AjaxCall',
        data: "function=doSpecLogin",
        async: false,
        cache: false,
        success: function (result) {
            res = result;
        },
        error: function () {

        }
    });
    return(res);
}
function getMetaIntegrity() {
    console.log("metaintegrity")
    var res = -100;
    var filenmsarr = "";
    for (var i = 0; i < tempArr.length; i++) {
        if (i === 0) {
            filenmsarr = tempArr[i].name;
        } else {
            filenmsarr = filenmsarr + "; " + tempArr[i].name;
        }
    }
    console.log(filenmsarr)
    getMetaIntegrityRC([{name: 'fileNms', value: filenmsarr}]);

    return res;
}

function handleMetaIntegrityResult(xhr, status, args) {
    var result = args.result;
    var uploadWidget = PF('importFile');
    uploadWidget.files = tempArr;
    tempArr = [];

    //metadata subset
    //include meta-data radiobutton
    if (result === "true") {
        uploadPermission = checkUploadPermission();
        if (uploadPermission === "false") {
            PF('uploadStatusDialog').show();
            setTimeout(refresh, 5000);
        } else {
            rc([{name: 'size', value: uploadWidget.files.length}]);
            PrimeFaces.widget.FileUpload.prototype.upload.call(uploadWidget);
        }
    } else {
        metaUploaded = false;
        showMetaErrorRC();
        uploadWidget.enableButton(uploadWidget.cancelButton);
    }
}


function uploadMeta() {
    /*
    var res = checkSessionUploaded();

    if (res) {
        PF('uploadSessionDialog').show();
        return false;
    }
     * /
     */
    var arr = PF('importFile').files;
    tempArr = [];
    var inx;
    var metaFile;
    var j = 0;
    //two meta files 
    for (var i = 0; i < arr.length; i++) {
        var file = arr[i];
        if (file.name.includes(".txt")) {
            inx = i;
            metaFile = arr[i];
            j++
        } else {
            tempArr.push(arr[i]);
        }
    }

    if (j > 1) {
        moreThanOneMetaRC();
        return false;
    } else if (metaFile !== null) {
        PF('importFile').files = [metaFile];
        return true;
    }
}

function refresh() {
    uploadPermission = checkUploadPermission();
    if (uploadPermission === "false") {
        setTimeout(refresh, 5000);
    } else {
        rc([{name: 'size', value: PF('importFile').files.length}]);
        PrimeFaces.widget.FileUpload.prototype.upload.call(PF('importFile'));
        PF('uploadStatusDialog').hide();
    }
}

function checkSessionUploaded() {
    var res = false;
    $.ajax({
        beforeSend: function () {

        },
        dataType: "html",
        type: "GET",
        url: '/MetaboAnalyst/faces/AjaxCall',
        data: "function=checkSessionUploaded",
        async: false,
        cache: false,
        success: function (result) {
            res = result;
            if (res === "true") {
                res = true;
            } else {
                res = false;
            }
            // console.log(res)
        },
        error: function () {

        }
    });
    return res;
}