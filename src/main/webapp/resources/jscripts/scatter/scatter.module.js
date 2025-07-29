/* 
 * 3D Scatter Plot 
 * XiaLab Analytics
 */

$(document).ready(function () {
    //NA is for initial loading, defined name is for updating (spectra processing module);
    initScatter3D("NA");
    updateTheme();
    initThemeSwitch();
    $(window).bind('resize', function () {
        setTimeout(resizeNetwork(), 300);
    });

    if (navigator.userAgent.indexOf("Firefox") > 0) {
        setDialogSize();
    }
});

import {
Object3D,
        LineSegments,
        EdgesGeometry,
        PlaneBufferGeometry,
        Box3,
        Fog,
        Vector3,
        Frustum,
        Matrix4,
        Color,
        Mesh,
        Line,
        Sphere,
        PlaneGeometry,
        MeshBasicMaterial,
        MeshLambertMaterial,
        SphereGeometry,
        BufferGeometry,
        BufferAttribute,
        Float32BufferAttribute,
        GridHelper,
        DoubleSide,
        Sprite,
        SpriteMaterial,
        MeshNormalMaterial,
        MeshPhysicalMaterial,
        CanvasTexture,
        LineBasicMaterial,
        MeshPhongMaterial,
        CubicBezierCurve3,
        CurvePath,
        Quaternion,
        VertexColors,
        TextureLoader,
        ConeGeometry,
        CylinderGeometry,
        TorusGeometry,
        DirectionalLight,
        BackSide,
        AmbientLight,
        SphereBufferGeometry,
        MOUSE,
        PCFSoftShadowMap,
        Scene,
        Group,
        ArrowHelper,
        MeshStandardMaterial,
        SpotLight,
        BoxGeometry,
        FontLoader,
        TextGeometry
        }
from "../three.module.js";

import { LineGeometry } from './lines/LineGeometry.js'
import ForceGraph3D from "./3d-force-graph.module.js"
import * as TWEEN from "./tween.esm.js";
import {mergeVertices} from './threejsmodules/BufferGeometryUtils.js';
import SpriteText from './three-spritetext.module.js';
import {ConvexGeometry} from './convexthree.module.js';
import {Rainbow} from './rainbowvis.js';
import * as legendUtils from "./legendOpts.js";
export {boundingClusters, initScatter3D, embeddedCollapsed, gData, scene2, takeImageBool, setTakeImageBool};

var savedState = {};
savedState.encasingFileNames = [];
var nodeSizeFactor = 2000;
var nodeSizeFactorLoading = 1200;
var gridsArr = [];
var gradCol1;
var gradCol2;
var sphericalRadius, initRadius, labelColOpt;
var scene2 = new Scene();
var naviString;
var current_selected_row;
var boundingClusters = {};
var comparison_omicstype = "";
var biplotTextColorU = "#ffffff";
var currentOmicsType = "";
var outlineBool = false;
var scene2, sphere2, sphere1, curr_meta;
var sphereOpacity = 0.8;
var sphereColor = "#d6d6d6";
var biplotArrows = {};
var biplotLabels = {};
var sphereMesh1;
var sphereMesh2;
var current_module = "NA";
var row_selection_on_user_click = true;
var current_editing_row = "";
var comp_last_selected_table = ""; //rows from which table to consider for comparison test
var labelArr = {};
var twoDMode = false;
var bundled = false;
var animationBool = false;
var compRes = [];
var lineObj = {};
var labelNodeMode = false;
var mainCheckBool = true;
var initScene2 = true;
var meta_arr = [];
var contours_info_obj = {};
var cluster_arr = [];
var current_halo_color = "#ffffff";
var current_encasing_color = "#ffffff";
var takeImageBool = false;
var embeddedCollapsed = false; //if condition is in 3d-forcegraph animation loop ctrl-f: setviewport
var current_main_plot = "score"; // score or loading in the main plot
var nav_arr = []; // array of meshes in navigation plot
var nav_nodes_arr = []; // array of nodes in nav plot
var shape_type = "contour";
var scatterType = "single";
var curShape = "sphere";
var planeU = {};
var cubeU = {};
var layerCol = "#666666";
var wallCol = "#666666";
var axisColorU = "#ffffff";
var arrowColorU = "#ff0000";
var axesArr = {};
var currentOrigin = v(0, 0, 0);
var Scatter, Scatter2 = "abc";
var sub_modules = "";
var selectedMeta = [];
var myTexts = {};
var clusterMeshes = {};
var boundingClustersGroup = {};
var explodedBool = false;
var labelMode = "global";
var nodes_num = 0;
var fixedLabels = false;
var htmlTextBool = false;
var current_point_color = '#e6194b';
var metaSphereBool = false;
var imagePath = '../../../resources/images/';
var distance2D = 50;
var boundingClustersMesh = {};
var takeVideo = false;
var renderingFinished = true;
var spriteysCanvas = {};
var animationVal = 601;
var radius = 1000,
        theta = 0;
var activeModules = {};
var initPos;
var labelContainerElem;
var bheight;
var curr_mode = "metasphere";
var mouseY;
var mouseX;
var elem;
var idsArr = [];
var camera;
var edgeWidth = 0;
var prevNode = "";
var colHover = "";
var highlightType = "halo";
var outlineUIs = {};
var labeling_threshold;
var curved = false;
var hideLabel = false;
var labelColor = "rgb(255,255,255)";
var labelColor2 = "black";
var camDefaultPos;
var bgColor = "#ffffff";
var spriteys = {};
var usr_dir;
var org;
var node_rows = [];
var node_rows_csize = [];
var highlightColor = '#0080ff';
var highlightSize = 30;
var currentEnrichFile = "";
var greyColor = '0x808080';
var focus_enr_anot;
var focus_fun_anot;
var current_fun_nodes;
var dim_mode = false;
var scope_mode = "single";
var nodes_number;
var planeLayer = false;
var planeArr = [];
var planeArr2 = [];
var current_color_attr = "degree";
var curr_omics = "omicsanalyst_0.json";
var nodeUIs = [];
var camDefaultPosClone;
var scopeh_mode = "single";
var edgeColor = "#d3d3d3";
var spherical = false;
var edgeOpacity = 0.8;
var nodeOpacity = 1;
var labelColSpectrum;
var viewMode = "default";
var bgColor2 = "#d3d3d3";
var isReductOptSingle = false;
var textNodeObj = {};
var analType;
var currFileNm;
var gData;
var fogBool;
var lastNodeClick = 0;
var algType = "";
function setTakeImageBool(value) {
    takeImageBool = value;
}

function initScatter3D(jsonNm) {
    console.log("parent===> " + parent);

    try {
        parent.PF("statusDialog").show();
    } catch (error) {
        console.error(error);
    }

    if (jsonNm === "NA") {
        jsonNm = parent.document.getElementById("mydir2").value;
    }
    usr_dir = parent.document.getElementById("userDir").value;
    curr_omics = "";
    org = "";
    analType = "";
    naviString = "Visual Analytics";
    labelColSpectrum = new Rainbow();
    labelColSpectrum.setSpectrum("#222222", "#ffffff");
    labelColSpectrum.setNumberRange(0, 100);

    setupNetworkFunctions();
    labelContainerElem = document.querySelector('#labels');

    $.getJSON('/MetaboAnalyst' + jsonNm, function (raw_data) {
        currFileNm = curr_omics;
        gData = raw_data;
        console.log(gData);
        if (gData.loading !== "NA" && gData.loading !== undefined) {
            gData.navigation = gData.loading;
            var arr;

            arr = [gData.nodes, gData.navigation];
            for (var j = 0; j < arr.length; j++) {
                for (var i = 0; i < arr[j].length; i++) {
                    var n = arr[j][i];
                    n.tcolor = n.colorb;
                    n.color = n.colorb;
                    if (arr[j] !== gData.navigation) {
                        n.tsize = (1 / Math.cbrt(arr[j].length)) * 120;
                        n.size = (1 / Math.cbrt(arr[j].length)) * 120;
                    } else {
                        if (gData.nodes.length > 500) {
                            n.tsize = n.size / 2;
                        } else {
                            n.tsize = n.size;
                        }
                    }
                    n.expcolor = n.expcolb;
                    n.opacity = 1;

                    delete n.x;
                    delete n.y;
                    delete n.z;
                    delete n.vx;
                    delete n.vy;
                    delete n.vz;
                }
            }
            gData["both"] = gData.nodes;
        } else {
            embeddedCollapsed = true;
            gData.navigation = "NA";
            if ($("#viewFocusOpt") !== undefined) {
                $("#viewFocusOpt").val("score");
                $("#viewFocusOpt").attr("disabled", true);
            }
            if ($("#bgColInsetOpt") !== undefined) {
                $("#bgColInsetOpt").attr("disabled", true);
            }

            if ($("#nodeOpt") !== undefined) {
                $("#nodeOpt").attr("disabled", true);
            }
        }

        var i = gData.nodes.length;
        while (i--) {
            if (gData.nodes[i].meta === "mcia.seg") {
                gData.nodes.splice(i, 1);
            }
        }

        for (var i = 0; i < gData.nodes.length; i++) {
            var n = gData.nodes[i];
            n.tcolor = n.colorb;
            n.color = n.colorb;
            n.size = 50;
            n.tsize = n.size;
            n.expcolor = n.expcolb;
            n.opacity = 1;

            delete n.x;
            delete n.y;
            delete n.z;
            delete n.vx;
            delete n.vy;
            delete n.vz;
        }

        if (gData.reductionOpt === "mcia") {
            for (var i = 0; i < gData.nodes.length; i++) {
                var n = gData.nodes[i];
                if (n.color === "#D3D3D3") {
                    n.size = 1;
                }
            }
        }

        gData.links = [];


        initMain("3d-graph");

        if (gData.navigation !== "NA") {
            $('#bgColInsetOpt').val("#000000");
            initScatter2();
            initGrids(scene2, Scatter.graphData().navigation, "loading", true);
            initGrids(scene2, Scatter.graphData().nodes, "main", false);
            mainCheckBool = false;
            changeBackground("#000000", "#000000");
            mainCheckBool = true;

            if (isReductOptSingle) {
                scene2.traverse(function (child) {
                    if (child instanceof Mesh) {
                        var n = child.userData.nodeData;
                        if (n !== undefined) {
                            if (!n.omicstype.includes(gData.omicstype[0])) {
                                child.visible = false;
                            } else {
                                child.visible = true;

                            }
                        }
                    }
                });
            }
        }

        if (Array.isArray(gData.meta)) {
            var newObj = {}
            newObj.Metadata = gData.meta;
            gData.meta = newObj;
        }

        Scatter.renderer().domElement.addEventListener('mousedown', function () {
            animationVal = 601;
        }, false);
        setupClusteringOpts();
        setTimeout(function () {
            initialSaveState(jsonNm);
        }, 200)
    });
}

function onNodeDoubleClick(node) {
    var d = new Date();
    var t = d.getTime();

    if ((t - lastNodeClick) < 500) {
        modifyNode(node);
    } else {
        // Aim at node from outside it
        //console.log(node);
    }
    lastNodeClick = t;
}

function initMain(elemName) {
    document.getElementById('loader_init').style.display = 'block';
    if (gData.misc !== "NA") {
        displayReductionInfo();
    }
    var container = document.getElementById(elemName);
    var elem = document.getElementById(elemName);

    Scatter = ForceGraph3D({
        controlType: "orbit"
    })
            (elem)
            .graphData(gData)
            .width(parseFloat(container.clientWidth))
            .linkOpacity(0)
            .linkWidth('width')
            .cooldownTicks(1) //Prevent force layout engine from running
            .nodeColor('color')
            .nodeOpacity(0.75)
            .nodeLabel('label')
            .nodeVal('size')
            .onNodeClick(onNodeDoubleClick).enableNodeDrag(false);

    Scatter.controls().staticMoving = true;

    const color = 0X222222; // white
    const near = 10;

    fogBool = false;
    //Scatter.backgroundColor("0xffffff");
    var camera = Scatter.camera();
    camera.position.x = 1700;
    camera.position.y = 1700;
    camera.position.z = 1700;

    camDefaultPos = {
        x: camera.position.x,
        y: camera.position.y,
        z: camera.position.z
    };
    camDefaultPosClone = camera.clone();
    Scatter.nodeResolution(16);
    var firstCanvas = document.getElementsByTagName("canvas")[0];
    firstCanvas.style.width = "1100px";
    firstCanvas.style.height = "840px";


    //setFDirected();
    initNodeSize();

    setTimeout(function () {
        initCube(Scatter.scene());
        axisColorU = "#222222";
        initCube(scene2);
        axisColorU = "#222222";
        turnOffFloor(true);

        if (gData.navigation === "NA") {
            $('#insetCheck').radiobutton({
                disabled: true
            });
            $('#mainCheck').radiobutton({
                disabled: true
            });
        }
        document.getElementById('loader_init').style.display = 'none';

        document.getElementById('loader').style.display = "none";


        // update scatter 
        if (gData.links.length !== 0) {
            Scatter.graphData().links.forEach(function (l) {
                colorLineObj2(l, l.color);
                opaLineObj(l, parseFloat(l.opacity));
            });
        }

        Scatter.graphData().nodes.forEach(function (nd) {
            nd.highlight = 0;
            colorNodeObj(nd, nd.tcolor);
            if (nd.meta === "mcia.seg") {
                opaNodeObj(nd, 0);
            } else {
                opaNodeObj(nd, 1);
            }
            materialNodeObj(nd, "MeshPhysicalMaterial");
            if (nd.shape !== undefined) {
                shapeNodeObj(nd, nd.shape);
            }
            deleteOutline(nd);
        });
        changeBackground("#000000", "#000000");
        Scatter.renderer().setAnimationLoop(updateLabelPositions);

        initRadius = computeSceneRadius() + 500;


        //Scatter.backgroundColor("#fff");
        //$('#p').panel('close')
    }, 1000);

    initGrids(Scatter.scene(), Scatter.graphData().nodes, "main", true);
    initGrids(Scatter.scene(), Scatter.graphData().navigation, "loading", false);

}


function initNodeAppearances() {
    setTimeout(function () {
        // update scatter 
        if (gData.links.length !== 0) {
            Scatter.graphData().links.forEach(function (l) {
                colorLineObj2(l, l.color);
                opaLineObj(l, parseFloat(l.opacity));
            });
        }
        Scatter.graphData().nodes.forEach(function (nd) {
            nd.highlight = 0;
            colorNodeObj(nd, nd.tcolor);
            if (nd.meta === "mcia.seg") {
                opaNodeObj(nd, 0);
            } else {
                opaNodeObj(nd, 1);
            }
            materialNodeObj(nd, "MeshPhysicalMaterial");
            deleteOutline(nd);
            if (nd.shape !== undefined) {
                shapeNodeObj(nd, nd.shape)
            }
        });
    }, 500);
}

var tempV = new Vector3();
var lpos = new Vector3();
var i_animation = 0;

function updateLabelPositions() {

    if (!hideLabel) {
        var propertyNms;
        if (htmlTextBool) {
            propertyNms = Object.keys(spriteys);
        } else {
            propertyNms = Object.keys(spriteysCanvas);
        }

        var nodes = Scatter.graphData().nodes;
        var camera = Scatter.camera();
        camera.updateMatrix();
        camera.updateMatrixWorld();
        var frustum = new Frustum();
        frustum.setFromProjectionMatrix(new Matrix4().multiplyMatrices(camera.projectionMatrix, camera.matrixWorldInverse));

        nodes.forEach(function (n) {
            if (propertyNms.indexOf(n.id) !== -1 && n.__threeObj !== undefined) {
                // get the position of the center of the cube
                var sphere = n.__threeObj;
                var distance = sphere.position.distanceTo(camera.position);
                sphere.updateWorldMatrix(true, false);
                sphere.getWorldPosition(tempV);
                tempV.project(camera);

                // convert the normalized position to CSS coordinates
                const x = (tempV.x * .5 + .5) * document.getElementById("3d-graph").clientWidth;
                const y = (tempV.y * -.5 + .5) * document.getElementById("3d-graph").clientHeight;

                // move the elem to that position
                var elem = spriteys[n.id];
                if (elem !== undefined && elem !== null) {
                    var textSprite = spriteysCanvas[n.id];
                    if (frustum.containsPoint(sphere.position)) {

                        if (htmlTextBool) {
                            elem.style.display = "block";
                            textSprite.visible = false;
                        } else {
                            elem.style.display = "none";
                            textSprite.visible = true;
                        }
                        if (htmlTextBool) {

                            elem.style.transform = `translate(-50%, -50%) translate(${x}px,${y}px)`;
                            var size = 1000 / distance * 100;
                            if (size > 120) {
                                elem.style.fontSize = 120 + "%";
                            } else {
                                if (size < 60) {
                                    elem.style.display = "none";
                                } else {
                                    elem.style.fontSize = size + "%";
                                    var rsize = rescale2Range(size, 60, 120, 0, 100);
                                    elem.style.color = labelColOpt; //'#' + labelColSpectrum.colourAt(rsize);
                                }
                            }
                        }


                        if (animationVal < 601) {
                            textSprite.visible = true;
                            elem.style.display = "none";
                        }
                    } else {
                        elem.style.display = "none";
                    }
                }

            }
        });
        updateLabel();
    }

    var camera = Scatter.camera();
// Create a new bounding box
    var screenSize;
// Traverse the group and update the bounding box
    var nodes = Scatter.graphData().nodes;
    nodes.forEach(function (n) {
        var mesh = n.__threeObj;

        if (mesh !== undefined && mesh.position !== undefined) {

            var distance = camera.position.distanceTo(mesh.position);
            var scale = distance / nodeSizeFactor; // 2600 is the initial camera position set initial node size initnodesize
            //nodeSizeFactor is useful for node size adjustment
            mesh.scale.set(scale, scale, scale);
        }
    });

    scene2.traverse(function (child) {
        if (child instanceof Mesh) {
            if (child.userData.nodeData !== undefined) {
                var distance = camera.position.distanceTo(child.position);
                var scale = distance / nodeSizeFactorLoading; // 2600 is the initial camera position set initial node size initnodesize
                child.scale.set(scale, scale, scale);

            }
        }
    });
}

var hoveredNode;

function hoverNode(node, prevNode) {
    if (hoveredNode && hoveredNode.__threeObj && hoveredNode.unhoveredColor) {
        if (hoveredNode.__threeObj.children[0]) {
            if (highlightType !== "halo") {
                colorNodeObj(hoveredNode, hoveredNode.unhoveredColor);
            }
        }
    }

    if (!node) {
        return;
    }

    if (node.focused === undefined) {
        return;
    }

    if (node.moduleInx !== undefined) {
        initPos = {
            x: node.position.x,
            y: node.position.y,
            z: node.position.z
        };
        Scatter.graphData().nodes.forEach(function (n) {
            n.initPos = {
                x: n.x,
                y: n.y,
                z: n.z
            };
        });
        var propertyNms = Object.keys(boundingClusters);
        for (var i = 0; i < propertyNms.length; i++) {
            var sphe = boundingClusters[propertyNms[i]];
            var sphe1 = boundingClustersMesh[propertyNms[i]];
            sphe.initPos = {
                x: sphe.position.x,
                y: sphe.position.y,
                z: sphe.position.z
            };
            sphe1.initPos = {
                x: sphe.position.x,
                y: sphe.position.y,
                z: sphe.position.z
            };
        }
        return;
    }

    if (node.__threeObj) {
        if (node.__threeObj.children[0]) {
            initPos = {
                x: node.x,
                y: node.y,
                z: node.z
            };
            Scatter.graphData().nodes.forEach(function (n) {
                n.initPos = {
                    x: n.x,
                    y: n.y,
                    z: n.z
                };
            });
            var propertyNms = Object.keys(boundingClusters);
            for (var i = 0; i < propertyNms.length; i++) {
                var sphe = boundingClusters[propertyNms[i]];
                sphe.initPos = {
                    x: sphe.position.x,
                    y: sphe.position.y,
                    z: sphe.position.z
                };
                var sphe1 = boundingClustersMesh[propertyNms[i]];
                sphe1.initPos = {
                    x: sphe1.position.x,
                    y: sphe1.position.y,
                    z: sphe1.position.z
                };
            }
            var newcol = LightenDarkenColor(node.color, -60);

            if (node.highlight !== 1) {
                node.unhoveredColor = node.color;
                colorNodeObj(node, newcol);
            }
            hoveredNode = node;


        }
    }
}

function modifyNode(node) {
    //node.unhoveredColor = highlightColor;

    //highlightNode(node);
    if (current_main_plot === "loading") {
        if (gData.metaShape !== undefined) {
            getTimeBoxPlot(node.id);

        } else {
            if (parent.PF("BoxPlotdialog") === undefined) {
                getBoxPlot(node.id);
            } else {
                setTimeout(function () {
                    getXICPlot(node.id);
                }, 0);
            }
        }
    } else {
        if (parent.PF("BoxPlotdialog") !== undefined) {
            setTimeout(function () {
                getaTICPlot(node.id);
            }, 0);
        }

    }
    console.log(node);
    //displayNodeInfo(node);

}

var vec = new Vector3();

function updateLabel() {

    var props = Object.keys(myTexts);

    if (props.length > 0) {
        for (var i = 0; i < props.length; i++) {
            var scaleVector = new Vector3();

            var sprite = myTexts[props[i]];
            var camera = Scatter.camera();
            var lengthFac = sprite.userData.id.length / 6;
            if (props[i].includes(scene2.uuid)) {
                var scaleFactor = 0.10;
                sprite.scale.x = lengthFac * scaleFactor * vec.setFromMatrixPosition(sprite.matrixWorld).sub(camera.position).length();
                sprite.scale.y = 0.030 * vec.setFromMatrixPosition(sprite.matrixWorld).sub(camera.position).length();
                sprite.scale.z = 1;
            } else {
                if (props[i].includes("tick@_")) {
                    var scaleFactor = 0.04;
                    sprite.scale.x = lengthFac * scaleFactor * vec.setFromMatrixPosition(sprite.matrixWorld).sub(camera.position).length();
                    sprite.scale.y = 0.016 * vec.setFromMatrixPosition(sprite.matrixWorld).sub(camera.position).length();
                    sprite.scale.z = 1;
                } else {
                    var scaleFactor = 0.06;
                    sprite.scale.x = lengthFac * scaleFactor * vec.setFromMatrixPosition(sprite.matrixWorld).sub(camera.position).length();
                    sprite.scale.y = 0.020 * vec.setFromMatrixPosition(sprite.matrixWorld).sub(camera.position).length();
                    sprite.scale.z = 1;
                }
            }
        }
    }

    if (Scatter === "abc") {
        var nodes = Scatter.graphData().nodes;
        for (var i = 0; i < nodes.length; i++) {
            var scaleVector = new Vector3();
            var scaleFactor = 0.0014;
            var sprite = nodes[i].__threeObj;
            var camera = Scatter.camera();
            if (sprite !== undefined) {
                if (vec.setFromMatrixPosition(sprite.matrixWorld).sub(camera.position).length() < 1000) {
                    sprite.scale.x = sprite.userData.scaling[0] * 0.001 * vec.setFromMatrixPosition(sprite.matrixWorld).sub(camera.position).length();
                    sprite.scale.y = sprite.userData.scaling[1] * 0.001 * vec.setFromMatrixPosition(sprite.matrixWorld).sub(camera.position).length();
                    sprite.scale.z = sprite.userData.scaling[2] * 0.001 * vec.setFromMatrixPosition(sprite.matrixWorld).sub(camera.position).length();
                } else {
                    sprite.scale.x = sprite.userData.scaling[0];
                    sprite.scale.y = sprite.userData.scaling[1];
                    sprite.scale.z = sprite.userData.scaling[2];
                }
            }
        }
        var zeroVec = v(0, 0, 0);
        Scatter.controls.panSpeed = zeroVec.sub(camera.position).length() / 250000;
    }
}

function updateColor() {
    setColorOpts(current_color_attr);
}

function highlightFunEnrichNodes(nodes, title, drawBorder) {

    if (nodes.length === 1) {
        searchNetwork(nodes[0]);
    } else {
        var nodeVec = [];
        highlight_mode = 1;
        if (current_main_plot === "score") {
            scene2.traverse(function (child) {
                if (child instanceof Mesh) {
                    if (child.userData.nodeData !== undefined) {

                        var id = child.userData.nodeData.id.split("_")[0];
                        var id2 = child.userData.nodeData.label.split("_")[0];

                        if (nodes.indexOf(id) !== -1 || nodes.indexOf(id2) !== -1) {
                            var colorValue = parseInt(highlightColor.replace("#", "0x"), 16);
                            var col = new Color(colorValue);
                            child.material.color = col;
                            child.userData.nodeData.highlight = 1;
                            child.material.opacity = 0.8;
                            child.visible = true;
                            nodeVec.push({
                                id: child.userData.nodeData.id,
                                label: child.userData.nodeData.label
                            });
                        } else {
                            if (child.userData.nodeData.highlight !== 1) {
                                child.material.transparent = true;
                                child.material.opacity = 0.15;
                                var colorValue = parseInt("#d3d3d3".replace("#", "0x"), 16);
                                var col = new Color(colorValue);
                                child.material.color = col;
                            }
                        }
                    }
                }
            });
        } else {
            Scatter.graphData().nodes.forEach(function (n) {
                if (nodes.indexOf(n.id) !== -1 || nodes.indexOf(n.label) !== -1) {
                    if (n.highlight !== 1) {
                        if (highlightType === "mixed") {
                            n.prevCol = n.color;
                            colorNodeObj(n, highlightColor);
                            opaNodeObj(n, 0.8);
                            n.highlight = 1;
                        } else {
                            highlightNode(n);
                            opaNodeObj(n, 1);
                            n.highlight = 1;
                        }
                    }
                    nodeVec.push({
                        id: n.id,
                        label: n.label
                    });

                } else {
                    if (n.highlight !== 1) {
                        n.prevCol = n.color;
                        //colorNodeObj(n, "#d3d3d3")
                        opaNodeObj(n, 0.2);
                    }
                }
            });
        }

        displayCurrentSelectedNodes(nodeVec, title);
    }
}

function highlightDirectLinks() {
    Scatter.graphData().links.forEach(function (l) {
        if (l.source.highlight === 1 && l.target.highlight === 1) {
            l.width = 3;
            sizeLineObj(l, l.width);
        }
    });
}


function unHighlightFunEnrichNodes(nodes) {
    if (nodes.constructor !== Array) {
        nodes = [nodes];
    }
    nodes = nodes.map(String);
    if (current_main_plot === "score") {
        scene2.traverse(function (child) {
            if (child instanceof Mesh) {
                var n = child.userData.nodeData;
                if (n !== undefined) {
                    if (nodes.indexOf(n.id) !== -1 || nodes.indexOf(n.label) !== -1) {
                        child.material.opacity = 0.2;
                        var colorValue = parseInt(child.userData.nodeData.colorb.replace("#", "0x"), 16);
                        var col = new Color(colorValue);
                        child.material.color = col;

                        child.visible = true;
                    }
                }
            }
        });
    } else {
        Scatter.graphData().nodes.forEach(function (n) {
            if (nodes.indexOf(n.id) !== -1 || nodes.indexOf(n.label) !== -1 || nodes.indexOf(n.keggId) !== -1) {
                if (highlightType === "mixed") {
                    n.highlight = 0;
                    if (n.prevCol === undefined || n.prevCol === null) {
                        colorNodeObj(n, n.tcolor);
                        opaNodeObj(n, 0.2);
                        n.color = n.tcolor;
                    } else {
                        colorNodeObj(n, n.prevCol);
                        opaNodeObj(n, 0.2);
                        n.color = n.prevCol;
                    }
                } else {
                    unhighlightNode(n);
                }

            }
        });
    }
    setEdgeColorGrey();
}

function setEdgeColorGrey() {
    Scatter.graphData().links.forEach(function (l) {
        colorLineObj(l, edgeColor);
    });
}

function displayHighNodes() {
    var node_vec = [];
    Scatter.graphData().nodes.forEach(function (nodeUI) {
        if (nodeUI.highlight === 1) {
            node_vec.push(nodeUI);
        }
    });
    displayCurrentSelectedNodes(node_vec, "");
}

function displayReductionInfo() {
    var stats = $("#stats");
    stats.empty();

    for (var propNm in gData.misc) {
        if (propNm === "pct") {
            stats.append('<strong>' + "Contributions(X,Y,Z)" + '</strong>: ' + gData.misc[propNm][0] + '%, ' + gData.misc[propNm][1] + '%, ' + gData.misc[propNm][2] + '%');
        } else if (propNm === "pct2") {
            for (var propNm in gData.misc.pct2) {
                if (propNm === currentOmicsType) {
                    stats.append('<strong>' + "Contributions(X,Y,Z)" + '</strong>: ' + gData.misc.pct2[propNm][0] + '%, ' + gData.misc.pct2[propNm][1] + '%, ' + gData.misc.pct2[propNm][2] + '%');
                }
            }
        } else {
            stats.append('<strong>' + propNm + '</strong>: ' + gData.misc[propNm] + "<br />");

        }
    }

}

function displayCurrentSelectedNodes(nodes, title) {
    var stats = $("#stats");
    //$('#p').panel('open')
    stats.empty();
    if (title !== "") {
        stats.append('<lh><b>' + title + '</b></lh>');
    }
    for (var i = 0; i < nodes.length; i++) {
        stats.append('<li class="stats">' + nodes[i].label + '</li>');
    }
}

function displayHighlightedNode(node) {

    var stats = $("#stats");
    //$('#p').panel('open')
    stats.append('<li class="stats">' + node.label + '</li>');

}

function updateCellColor(color, id) {
    $("#" + id).css("background-color", color.replace('0x', '#'));
}

function pagerFilter(data) {
    if (typeof data.length === 'number' && typeof data.splice === 'function') { // is array
        data = {
            total: data.length,
            rows: data
        };
    }
    var dg = $('#dg');
    var opts = dg.datagrid('options');
    var pager = dg.datagrid('getPager');
    pager.pagination({
        showPageList: false,
        showRefresh: false,
        displayMsg: "",
        onSelectPage: function (pageNum, pageSize) {
            opts.pageNumber = pageNum;
            opts.pageSize = pageSize;
            pager.pagination('refresh', {
                pageNumber: pageNum,
                pageSize: pageSize
            });
            dg.datagrid('loadData', data);
        }
    });
    if (!data.originalRows) {
        data.originalRows = (data.rows);
    }
    var start = (opts.pageNumber - 1) * parseInt(opts.pageSize);
    var end = start + parseInt(opts.pageSize);
    data.rows = (data.originalRows.slice(start, end));
    return data;
}

function searchNetwork(nodeID) {
    var hit = 0;
    if (current_main_plot === "score") {
        Scatter.graphData().nodes.forEach(function (n) {
            if (n.id === nodeID || n.label === nodeID) {
                hit = 1;
                n.highlight = 1;
                opaNodeObj(n, 0.8);
                shiftCameraToNode(n);


                if (highlightType === "mixed") {
                    n.prevCol = n.color;
                    if (outlineUIs[n.id] === null || outlineUIs[n.id] === undefined) {
                        outlineNode(n, highlightColor);
                    } else {
                        deleteOutline(n);
                        outlineNode(n, highlightColor);
                    }
                } else {
                    highlightNode(n);
                }
                displayNodeInfo(n);
            }
        });
    } else {
        Scatter.graphData().nodes.forEach(function (n) {
            if (n.id === nodeID || n.label === nodeID) {
                hit = 1;
                n.highlight = 1;
                opaNodeObj(n, 1);
                if (highlightType === "mixed") {
                    n.prevCol = n.color;
                    if (outlineUIs[n.id] === null || outlineUIs[n.id] === undefined) {
                        outlineNode(n, highlightColor);
                    } else {
                        deleteOutline(n);
                        outlineNode(n, highlightColor);
                    }
                } else {
                    highlightNode(n);
                }
                displayNodeInfo(n);
            } else {
                if (n.highlight !== 1) {
                    deleteOutline(n);
                    opaNodeObj(n, 0.15);
                }
                //colorNodeObj(n, "#d3d3d3")
            }
        });
    }
    //setEdgeColorGrey();
    if (!hit) {
        $.messager.alert('Error', "Node " + nodeID + " was not found in the current network!", 'error');
    }
    return;
}


function resetNodes(nodeIDs) {
    var count = 0;
    Scatter.graphData().nodes.forEach(function (n) {
        if (nodeIDs.indexOf(n.id) !== -1) {
            if (outlineUIs[n.id] !== undefined || outlineUIs[n.id] !== null) {
                deleteOutline(n);
            }

            if (highlightType === "mixed") {
                n.highlight = 0;
                if (n.prevCol === undefined || n.prevCol === null) {
                    colorNodeObj(n, n.tcolor);
                    n.color = n.tcolor;
                } else {
                    colorNodeObj(n, n.prevCol);
                    n.color = n.prevCol;
                }
            } else {
                unhighlightNode(n);
            }
        }

        if (n.highlight === 1) {
            count++;
        }
    });

    if (count === 0) {
        updateNode();
    }
}

function highlightMyNodes() {
    var ids = $('#batch').val().split('\n');
    highlightNodes(ids, highlightType);
}

function unhighlightMyNodes() {
    var ids = $('#batch').val().split('\n');
    unhighlightNodes(ids);
}

function unhighlightNodes(ids) {
    Scatter.graphData().nodes.forEach(function (n) {
        if (ids.indexOf(n.id) !== -1 || ids.indexOf(n.label) !== -1) {
            colorNodeObj(n, n.tcolor);
            if (outlineUIs[n.id] !== null || outlineUIs[n.id] !== undefined) {
                deleteOutline(n);
            }
            n.highlight = 0;
        }
    });
    //setEdgeColorGrey();
}

function highlightNodes(ids, type) {
    Scatter.graphData().nodes.forEach(function (n) {
        if (ids.indexOf(n.id) !== -1 || ids.indexOf(n.label) !== -1) {
            if (type === "node" || type === "mixed") {
                n.prevCol = n.color;
                n.highlight = 1;
                colorNodeObj(n, highlightColor);
            } else {
                highlightNode(n);
            }
        } else {
            colorNodeObj(n, "#d3d3d3");
            opaNodeObj(n, 0.2);
        }
    });
    //setEdgeColorGrey();
    displayHighNodes();
}

function displayNodeInfo(node) {
    var stats = $("#stats");
    stats.empty();
    if (node.type === "peak") {
        stats.append('<strong>Name</strong>: ' + node.label);
    } else {
        stats.append('<li class="stats"><strong>Name</strong>: ' + node.label + '</li>');
    }
    if (node.type === "gene" || node.type === "interactor" || node.type === "tf") {
        stats.append('<li class="stats"><strong>Entrez</strong>: ' + '<a href="https://www.ncbi.nlm.nih.gov/gene/' + node.id + '" target="_blank"><u>' + node.id + '</u></a>' + '</li>');
    } else if (node.type === "met") {
        stats.append('<li class="stats"><strong>KEGG</strong>: ' + '<a href="http://www.genome.jp/dbget-bin/www_bget?' + node.id + '" target="_blank"><u>' + node.id + '</u></a>' + '</li>');
    } else if (node.type === "mir") {
        stats.append('<li class="stats"><strong>mirBase</strong>: ' + '<a href="http://www.mirbase.org/cgi-bin/mature.pl?mature_acc=' + node.id + '" target="_blank"><u>' + node.id + '</u></a>' + '</li>');
    } else if (node.type === "peak") {
        Scatter.graphData().links.forEach(function (l) {
            if (l.source.id === node.id) {
                stats.append('<li class="stats"><strong>' + l.target.label + '</strong>:' + l.label + '</li>');
            } else if (l.target.id === node.id) {
                stats.append('<li class="stats"><strong>' + l.source.label + '</strong>:' + l.label + '</li>');
            }
        });

    }
}

function displayNodeInfoMeta(node) {
    $("#dragDiv").css('display', 'none');
    var stats = $("#stats");
    var title = node.id;
    var nodes = [];

    stats.empty();
    if (title !== "") {
        stats.append('<lh><b>' + title + '</b></lh>');
    }
    for (var i = 0; i < node.metanodes.length; i++) {
        stats.append('<li class="stats">' + node.metanodes[i].label + '</li>');
    }
}

function displayNodeInfoModule(inx) {
    $("#dragDiv").css('display', 'none');
    var stats = $("#stats");
    var title = "module" + inx;
    var nodes = [];
    Scatter.graphData().nodes.forEach(function (n) {
        if (n.moduleNumber + "" === inx + "") {
            nodes.push(n);
        }
    });

    stats.empty();
    if (title !== "") {
        stats.append('<lh><b>' + title + '</b></lh>');
    }
    for (var i = 0; i < nodes.length; i++) {
        stats.append('<li class="stats">' + nodes[i].label + '</li>');
    }
}

function updateNode() {
    var stats = $("#stats");
    stats.empty();
    var col2 = $('#dg2').datagrid('getColumnOption', 'color');
    col2.styler = function () {
        return 'background-color:white';
    };
}


function exportResultTable(name) {
    if (name === "funtb") {
        if ($('#dge').datagrid('getRows').length === 0) {
            $.messager.alert('Error', 'No functional enrichment analysis has been performed!', 'error');
            return;
        }
    }
    if (name === "comtb") {
        setupFileDownload("module_table.csv");
    } else if (name === "funtb") {
        setupFileDownload(currentEnrichFile + ".csv");
    } else {
        doGraphExport(setupFileDownload, name);
    }
}

function setupFileDownload(result) {
    var fileLnk = $("#fileLnk");
    fileLnk.empty();
    fileLnk.append("Right click the link below, then 'Save Link As ... ' to download the file<br/><br/>");
    fileLnk.append('<strong><a href="' + usr_dir + '/' + result + '" target="_blank"><u>' + result + '</u></a></strong>');
    $.messager.progress('close');
    $("#filedialog").dialog('open');
}

function setupNetworkFunctions() {
    initReportFunctions();
    $('#form1 #updateLoading', parent.document).bind('click keypress', function (event) {
        console.log($('#form1 #loadingSlider', parent.document));
        var val = $('#form1 #loadingSlider', parent.document).val();
        var opt = $('#form1 #pcaDisplay', parent.document).val();
        updateLoading(opt, val);
    });


    $('#nodeLabelBn').bind('click keypress', function (event) {
        var val = $("#labelOpt").val();

        if (val === "show") {
            fixedLabels = false;
            hideLabel = false;

            for (var propertyName in myTexts) {
                //spriteys[propertyName].style.display = "block";
                myTexts[propertyName].material.visible = true;
            }


        } else {
            fixedLabels = false;
            hideLabel = true;
            for (var propertyName in myTexts) {
                //spriteys[propertyName].style.display = "none";
                if (myTexts[propertyName] !== undefined) {
                    myTexts[propertyName].material.visible = false;
                }
            }
        }
        $('#optdlg').dialog('close');
    });

    $("#axisOpt").val("bottom");

    $('#batchBn').bind('click keypress', function (event) {
        highlightMyNodes();
    });

    window.addEventListener('resize', onWindowResize, false);
    $('#nodeSizeBn').bind('click keypress', function (event) {

        var type = $('#nodeOpt').val();
        var val = $('#sizeOpt').val();
        console.log(type);
        if (val === "increase") {
            if (type === "inset") {
                nodeSizeFactorLoading = nodeSizeFactorLoading * 0.95;
            } else if (type === "main") {
                nodeSizeFactor = nodeSizeFactor * 0.95;
            } else {
                nodeSizeFactorLoading = nodeSizeFactorLoading * 0.95;
                nodeSizeFactor = nodeSizeFactor * 0.95;
            }
            if (viewMode.includes("text")) {
                Scatter.graphData().nodes.forEach(function (n) {
                    setTextNodeSize(n.__threeObj, true);
                });
            }

        } else {
            if (type === "inset") {
                nodeSizeFactorLoading = nodeSizeFactorLoading * 1.05;
            } else if (type === "main") {
                nodeSizeFactor = nodeSizeFactor * 1.05;
            } else {
                nodeSizeFactorLoading = nodeSizeFactorLoading * 1.05;
                nodeSizeFactor = nodeSizeFactor * 1.05;
            }
            if (viewMode.includes("text")) {
                Scatter.graphData().nodes.forEach(function (n) {
                    setTextNodeSize(n.__threeObj, false);
                });
            }
        }
    });

    $('#nodeSizeBn2').bind('click keypress', function (event) {
        var curType = current_module.split(/_(.+)/)[0];
        var curSelection = current_module.split(/_(.+)/)[1];
        if (curType === "cluster") {
            var row = $('#mdg').datagrid('getRows')[curSelection];
        } else {
            var row = $('#metadg').datagrid('getRows')[curSelection];
        }
        var ids_arr = row.ids;
        var val = $('#sizeOpt2').val();

        if (val === "increase") {
            Scatter.graphData().nodes.forEach(function (n) {
                if (ids_arr.indexOf(n.id) !== -1) {
                    sizeNodeObj(n, n.__threeObj.scale.x * 1.1);
                }
            });
        } else {
            Scatter.graphData().nodes.forEach(function (n) {
                if (ids_arr.indexOf(n.id) !== -1) {
                    sizeNodeObj(n, n.__threeObj.scale.x * 0.9);
                }
            });
        }
    });

    $('#moreOpts').bind('click keypress', function (event) {
        $('#optdlg').dialog('open');
        $('#optdlg').window('moveTo', 'center')
    });

    $('#clusterBn').bind('click keypress', function (event) {
        performClusteringScatter();
    });

    $('#targetedClusteringBn').bind('click keypress', function (event) {
        performTargetedClustering();
        $('#highlightdlg').dialog('close');
    });

    $('#compBn').bind('click keypress', function (event) {
        performTtest();
    });

    $('#enrBn').bind('click keypress', function (event) {
        testEnrichment();
    });

    $('#enrResBn').bind('click keypress', function (event) {
        exportResultTable('funtb');
    });

    $('#updateCustomSelectionBn').bind('click keypress', function (event) {
        updateCustomSelection();
        $('#highlightdlg').dialog('close');
    });

    $('#animBn').bind('click keypress', function (event) {
        startAnimation();
    });

    $('#updateSphereBn').bind('click keypress', function (event) {
        changeSphere();
    });

    $('#batchHighlightBn').bind('click keypress', function (event) {
        highlightMyNodes();
    });

    $('#batchResetBn').bind('click keypress', function (event) {
        unhighlightMyNodes();
    });

    $('#resetBiplotBn').bind('click keypress', function (event) {
        resetBiplotBn();
    });

    $('#biplotBn').bind('click keypress', function (event) {
        biplotScore();
    });

    $('#clearContour').bind('click keypress', function (event) {
        clearContour();
    });

    $('#biplotBn2').bind('click keypress', function (event) {
        biplotLoading();
    });

    $('#resetBiplotBn3').bind('click keypress', function (event) {
        clearContour();
    });

    $('#switchToScoreBn').bind('click keypress', function (event) {
        switchToScoreFromDialog();
    });

    $('#copyShareLinkBn').bind('click keypress', function (event) {
        copyShareLink();
    });

    $('#setShadingBn').bind('click keypress', function (event) {
        setShading();
        $('#shadingdlg').dialog('close');
    });


    $('#compopt').change(function () {
        var opt = $('#compopt');
        if (opt.val() === "cluster") {
            curr_meta = opt.val();
        } else {
            curr_meta = $('#metaopt').val();
        }
    });

    $('#shareBn').bind('click keypress', function (event) {
        sharingLink(org, analType, naviString);
    });

    $('#exportOpt').change(function () {
        var val = $(this).val();
        if (val === "png") {
            setTimeout(function () {
                takeScreenshot();
                takeImageBool = false;
            }, 100);
        } else if (val === "gif") {
            $('#animationdlg').dialog('open');
        }
    });


    $('#presetColOpt').change(function () {
        var val = $(this).val();
        if (val === viewMode) {
            return;
        }
        $('#loader').show();
        if (val === "text") {
            if (viewMode === "textMeta") {
                Scatter.graphData().nodes.forEach(function (nd) {
                    //opaNodeObjForLabelToggle(nd, 1)
                    removeTextNodeObj(nd.__threeObj);
                    //colorNodeObj(n, n.tcolor);
                });
                hideLabel = false;
            }
            hideLabel = true;
            Scatter.graphData().nodes.forEach(function (nd) {
                if (nd.meta !== "mcia.seg" && nd.metatype !== undefined) {
                    opaNodeObjForLabelToggle(nd, 0);
                    addTextNodeObj(nd, 48, "name"); // scene1 or scene2
                }
            });

        } else if (val === "textMeta") {
            if (viewMode === "text") {
                Scatter.graphData().nodes.forEach(function (nd) {
                    //opaNodeObjForLabelToggle(nd, 1)
                    removeTextNodeObj(nd.__threeObj);
                    //colorNodeObj(n, n.tcolor);
                });
                hideLabel = false;
            }
            hideLabel = true;
            Scatter.graphData().nodes.forEach(function (nd) {
                if (nd.meta !== "mcia.seg" && nd.metatype !== undefined) {
                    opaNodeObjForLabelToggle(nd, 0);
                    addTextNodeObj(nd, 48, "meta"); // scene1 or scene2
                }
            });

        } else {
            //need to go back to node view
            if (viewMode === "text" || viewMode === "textMeta") {
                Scatter.graphData().nodes.forEach(function (nd) {
                    opaNodeObjForLabelToggle(nd, 1);
                    removeTextNodeObj(nd.__threeObj);
                    //colorNodeObj(n, n.tcolor);
                });
                hideLabel = false;
            }

            //var nms = Object.keys(boundingClusters)
            //for (var i = 0; i < nms.length; i++) {
            //    var nm = nms[i]
            //    deleteMetaSphere(nm, Scatter.scene())
            //}

            if (val === "plain") {
                Scatter.graphData().nodes.forEach(function (n) {
                    colorNodeObj(n, "#d6d6d6");
                });
            } else { //default
                Scatter.graphData().nodes.forEach(function (nd) {
                    colorNodeObj(nd, nd.tcolor);
                });
            }
        }
        viewMode = val;
        $('#loader').hide();
    });

    $('#bgColorOptions').change(function () {
        var value = $(this).val();
        var group = value.split('-')[0]; // 'main' or 'inset'
        var color = value.split('-')[1]; // '#ffffff' or '#000000'
        if (group === "main") {
            mainCheckBool = true;
            changeBackground(color, color);
        } else {
            mainCheckBool = false;
            changeBackground(color, color);
            mainCheckBool = true;
        }


    });

    $('#tickDisplayOpt').change(function () {
        var val = $(this).val();
        if (val === "yes") {
            for (var propertyName in myTexts) {
                if (propertyName.includes("tick@_")) {
                    myTexts[propertyName].material.visible = true;
                }
            }
        } else {
            for (var propertyName in myTexts) {
                if (propertyName.includes("tick@_")) {
                    console.log("abc")
                    myTexts[propertyName].material.visible = false;
                }
            }
        }
    });

    $('#styleOpt').change(function () {
        var val = $(this).val();
        var url = usr_dir + "/" + curr_omics;
        if (val === "na") {
            return;
        } else if (val === "label") {
            $('#nodelabeldlg').dialog('open');
        } else if (val === "color") {
            $('#nodecoldlg').dialog('open');
        } else if (val === "size") {

            $('#nodestyledlg').dialog('openCenter');
            $('#ttNode').tabs("select", "Size");

        } else if (val === "shade") {
            $('#shadingdlg').dialog('open');
        } else if (val === "shape") {
            $('#nodestyledlg').dialog('openCenter');

            $('#ttNode').tabs("select", "Shape");
        } else if (val === "opacity") {
            if (viewMode.includes("text")) {
                $.messager.alert('Error', 'Not available for text-based node display. Please switch to another mode before using this feautre.', 'error');
                return;
            }
            $('#nodestyledlg').dialog('openCenter');
            $('#ttNode').tabs("select", "Opacity");
        } else if (val === "topo") {
            if (curr_mode === "single") {
                changeNet(url);
            }
            $('#nodecoldlg').dialog('open');
        } else {

            $('#loader').show();
            if (val === "text") {
                if (viewMode === "textMeta" || viewMode === "textMeta2") {
                    Scatter.graphData().nodes.forEach(function (nd) {
                        removeTextNodeObj(nd.__threeObj);
                    });
                    hideLabel = false;
                }
                hideLabel = true;
                Scatter.graphData().nodes.forEach(function (nd) {
                    console.log(nd)
                    if (nd.meta !== "mcia.seg" && nd.meta !== undefined) {
                        opaNodeObjForLabelToggle(nd, 0);
                        addTextNodeObj(nd, 48, "name"); // scene1 or scene2
                    }
                });

            } else if (val === "textMeta") {
                if (viewMode === "text" || viewMode === "textMeta2") {
                    Scatter.graphData().nodes.forEach(function (nd) {
                        removeTextNodeObj(nd.__threeObj);
                    });
                    hideLabel = false;
                }
                hideLabel = true;
                Scatter.graphData().nodes.forEach(function (nd) {
                    if (nd.meta !== "mcia.seg" && nd.meta !== undefined) {
                        opaNodeObjForLabelToggle(nd, 0);
                        addTextNodeObj(nd, 48, "meta"); // scene1 or scene2
                    }
                });

            } else if (val === "textMeta2") {
                if (viewMode === "text" || viewMode === "textMeta") {
                    Scatter.graphData().nodes.forEach(function (nd) {
                        removeTextNodeObj(nd.__threeObj);
                    });
                    hideLabel = false;
                }
                hideLabel = true;
                Scatter.graphData().nodes.forEach(function (nd) {
                    if (nd.meta !== "mcia.seg" && nd.meta !== undefined) {
                        opaNodeObjForLabelToggle(nd, 0);
                        addTextNodeObj(nd, 48, "meta2"); // scene1 or scene2
                    }
                });

            } else {
                //need to go back to node view
                if (viewMode === "text" || viewMode === "textMeta") {
                    Scatter.graphData().nodes.forEach(function (nd) {
                        opaNodeObjForLabelToggle(nd, 1);
                        removeTextNodeObj(nd.__threeObj);
                        //colorNodeObj(n, n.tcolor);
                    });
                    hideLabel = false;
                }

                //var nms = Object.keys(boundingClusters)
                //for (var i = 0; i < nms.length; i++) {
                //    var nm = nms[i]
                //    deleteMetaSphere(nm, Scatter.scene())
                //}

                if (val === "plain") {
                    Scatter.graphData().nodes.forEach(function (n) {
                        colorNodeObj(n, "#d6d6d6");
                    });
                } else { //default
                    Scatter.graphData().nodes.forEach(function (nd) {
                        colorNodeObj(nd, nd.tcolor);
                    });
                }
            }
            viewMode = val;
            $('#loader').hide();
        }
    });

    $('#viewFocusOpt').change(function () {
        var val = $(this).val();

        embeddedCollapsed = true;

        if (val === "joint") {
            embeddedCollapsed = false;
        } else if (val === "biplot") {
            $('#biplotdlg1').dialog('open');
        } else if (val === "biplot-load") {
            $('#biplotdlg2').dialog('open');
        } else {
            $('#loader').show();
            if (val !== current_main_plot) {
                if (val === "score") {
                    switchToScore();
                } else {
                    switchToLoading();
                }
            }

            var nms = Object.keys(boundingClusters);
            for (var i = 0; i < nms.length; i++) {
                var nm = nms[i];
                deleteMetaSphere(nm, Scatter.scene());
            }

            var nms = Object.keys(biplotArrows);
            for (var i = 0; i < nms.length; i++) {
                var nm = nms[i];
                deleteMesh(biplotArrows[nm], Scatter.scene());
                delete biplotArrows[nm];
            }
            nms = Object.keys(biplotLabels);
            for (var i = 0; i < nms.length; i++) {
                var nm = nms[i];
                deleteMesh(biplotLabels[nm], Scatter.scene());
                delete biplotLabels[nm];
            }
            setTimeout(function () {
                $('#loader').hide();
            }, 10);
        }

        outlineBool = false;

    });


    $('#viewSelOpt').change(function () {
        var val = $(this).val();
        if (val === viewMode) {
            return;
        }

        if (val === "auto") {
            if (!isReductOptSingle) {
                document.getElementById('biplotRank1').style.display = "table-row";
                document.getElementById('biplotRank2').style.display = "table-row";
            }
            document.getElementById('biplotBatch').style.display = "none";
        } else if (val === "custom") {
            document.getElementById('biplotRank1').style.display = "none";
            document.getElementById('biplotRank2').style.display = "none";
            document.getElementById('biplotBatch').style.display = "table-row";
        }
        viewMode = val;
    });


    $('#resetBn').bind('click keypress', function (event) {
        removeAllOverlays();
        resetNetwork();
    });


    $('#switchBn').bind('click keypress', function (event) {
        if (gData.navigation === "NA") {
            $.messager.alert('', "Only score plot is available for this dimension reduction algorithm", 'error');
            return;
        }
        if (current_main_plot === "score") {
            switchToLoading();
        } else {
            switchToScore();
        }
    });


    $('#collapseBn').bind('click keypress', function (event) {
        if (gData.navigation === "NA") {
            $.messager.alert('', "Only score plot is available for this dimension reduction algorithm", 'error');
            return;
        }
        if (embeddedCollapsed) {
            embeddedCollapsed = false;
        } else {
            embeddedCollapsed = true;
        }

    });


    $('#nodeDisplayOpt').change(function () {
        //need to go back to node view
        $('#loader').show();
        var val = $("#nodeDisplayOpt").val();
        console.log(val);
        if (val === "text") {
            if (viewMode === "textMeta" || viewMode === "textMeta2") {
                Scatter.graphData().nodes.forEach(function (nd) {
                    removeTextNodeObj(nd.__threeObj);
                });
                hideLabel = false;
            }
            hideLabel = true;
            Scatter.graphData().nodes.forEach(function (nd) {
                console.log(nd)
                if (nd.meta !== "mcia.seg" && nd.meta !== undefined) {
                    opaNodeObjForLabelToggle(nd, 0);
                    addTextNodeObj(nd, 48, "name"); // scene1 or scene2
                }
            });

        } else if (val === "textMeta") {
            if (viewMode === "text" || viewMode === "textMeta2") {
                Scatter.graphData().nodes.forEach(function (nd) {
                    removeTextNodeObj(nd.__threeObj);
                });
                hideLabel = false;
            }
            hideLabel = true;
            Scatter.graphData().nodes.forEach(function (nd) {
                if (nd.meta !== "mcia.seg" && nd.meta !== undefined) {
                    opaNodeObjForLabelToggle(nd, 0);
                    addTextNodeObj(nd, 48, "meta"); // scene1 or scene2
                }
            });

        } else if (val === "textMeta2") {
            if (viewMode === "text" || viewMode === "textMeta") {
                Scatter.graphData().nodes.forEach(function (nd) {
                    removeTextNodeObj(nd.__threeObj);
                });
                hideLabel = false;
            }
            hideLabel = true;
            Scatter.graphData().nodes.forEach(function (nd) {
                if (nd.meta !== "mcia.seg" && nd.meta !== undefined) {
                    opaNodeObjForLabelToggle(nd, 0);
                    addTextNodeObj(nd, 48, "meta2"); // scene1 or scene2
                }
            });

        } else {
            //need to go back to node view
            if (viewMode === "text" || viewMode === "textMeta") {
                Scatter.graphData().nodes.forEach(function (nd) {
                    opaNodeObjForLabelToggle(nd, 1);
                    removeTextNodeObj(nd.__threeObj);
                    //colorNodeObj(n, n.tcolor);
                });
                hideLabel = false;
            }

            //var nms = Object.keys(boundingClusters)
            //for (var i = 0; i < nms.length; i++) {
            //    var nm = nms[i]
            //    deleteMetaSphere(nm, Scatter.scene())
            //}

            if (val === "plain") {
                Scatter.graphData().nodes.forEach(function (n) {
                    colorNodeObj(n, "#d6d6d6");
                });
            } else { //default
                Scatter.graphData().nodes.forEach(function (nd) {
                    colorNodeObj(nd, nd.tcolor);
                });
            }
        }
        viewMode = val;
        $('#loader').hide();
    });

    $('#rotateMoveBn').bind('click keypress', function (event) {
        var controls = Scatter.controls();
        controls.mouseButtons = {
            LEFT: MOUSE.LEFT,
            MIDDLE: MOUSE.MIDDLE,
            RIGHT: MOUSE.RIGHT
        };
    });

    $('#planeMoveBn').bind('click keypress', function (event) {
        var controls = Scatter.controls();
        controls.mouseButtons = {
            LEFT: MOUSE.RIGHT,
            MIDDLE: MOUSE.MIDDLE,
            RIGHT: MOUSE.LEFT
        };
    });

    $('#selectBn').bind('click keypress', function (event) {
        initFilter();
        $('#filterdlg').dialog('open');
    });

    console.log($('#form11 #updatePcaBn', parent.document));
    $('#form11 #updatePcaBn', parent.document).bind('click keypress', function (event) {
        console.log("update=====")
    });


    var timeout;
    $('#zoomInBn').bind('mousedown', function (event) {
        timeout = setInterval(function () {
            zoomIn(50);
            zoomCheck();
            updateLabel();
        }, 5);

        return false;
    });

    $('#zoomInBn').bind('mouseup', function (event) {
        clearInterval(timeout);
        return false;
    });

    $('#zoomOutBn').bind('mouseup', function (event) {
        clearInterval(timeout);
        return false;
    });

    $('#zoomOutBn').bind('mousedown', function (event) {
        timeout = setInterval(function () {
            zoomOut(50);
            zoomCheck();
            updateLabel();
        }, 5);
    });

    $('#updateWidth').bind('click keypress', function (event) {
        if (gData.links.length === 0) {
            $.messager.alert('Error', 'No edges are detected within the scatter plot!', 'error');
            return;
        }
        var val = parseFloat($('#edgeSliderWidth').val());
        edgeWidth = val;
        Scatter.graphData().links.forEach(function (l) {
            l.width = edgeWidth;
            sizeLineObj(l, edgeWidth);
            colorLineObj2(l, l.color);
        });

        $('#widthdlg').dialog('close');
    });


    $("#custom").spectrum({
        color: highlightColor,
        showInitial: true,
        change: function (color) {
            highlightColor = color.toHexString();
        },
        showInput: true,
        showPalette: true,
        showSelectionPalette: true, // true by default
        palette: [
            ['black', 'white'],
            ['#666666', '#0080ff']
        ]
    });

    $("#customAxis").spectrum({
        color: "#ffffff",
        showInitial: true,
        change: function (color) {
            axisColorU = color.toHexString();
            var sc;
            if (mainCheckBool) {
                sc = Scatter.scene();
            } else {
                sc = scene2;
            }
            generateAxes(sc, 1100);
        },
        showInput: true,
        showPalette: true,
        showSelectionPalette: true, // true by default
        palette: [
            ['black', 'white'],
            ['#666666', '#0080ff']
        ]
    });
    $("#customShadow").spectrum({
        disabled: true,
        color: "#D3D3D3"
    });

    $('#encasingCheckbox').change(function () {
        var sc;
        if (mainCheckBool) {
            sc = Scatter.scene();
        } else {
            sc = scene2;
        }

        if (this.checked) {
            switchToScoreFromDialog();
        } else {
            clearContour();
        }

    });


    $("#customSphere").spectrum({
        color: sphereColor,
        showInitial: true,
        change: function (color) {
            var col = color.toHexString();
            sphereColor = col;
        },
        showInput: true,
        showPalette: true,
        showSelectionPalette: true, // true by default
        palette: [
            ['black', 'white'],
            ['#666666', '#0080ff']
        ]
    });

    $("#customWall").spectrum({
        color: "#666666",
        showInitial: true,
        change: function (color) {
            wallCol = color.toHexString();

            var sc;
            if (mainCheckBool) {
                sc = Scatter.scene();
            } else {
                sc = scene2;
            }

            if ($('#wlCheckbox')[0].checked) {
                initWall(sc);
            }

        },
        showInput: true,
        showPalette: true,
        showSelectionPalette: true, // true by default
        palette: [
            ['black', 'white'],
            ['#666666', '#0080ff']
        ]
    });

    $("#customLayer").spectrum({
        color: "#666666",
        showInitial: true,
        change: function (color) {
            layerCol = color.toHexString();

            var sc;
            if (mainCheckBool) {
                sc = Scatter.scene();
            } else {
                sc = scene2;
            }

            if ($('#flCheckbox')[0].checked) {
                initPlane(sc);
            }

        },
        showInput: true,
        showPalette: true,
        showSelectionPalette: true, // true by default
        palette: [
            ['black', 'white'],
            ['#666666', '#0080ff']
        ]
    });

    $('#dldBn').bind('click keypress', function (event) {
        setTimeout(function () {
            takeScreenshot();
            takeImageBool = false;

        }, 100);
    });



    $('#encasingBn').bind('click keypress', function (event) {
        if (current_main_plot === "score") {
            console.log(boundingClusters);
            if (Object.keys(boundingClusters).length === 0) {
                switchToScoreFromDialog();
            } else {
                clearContour();
            }

        } else {
            $.messager.alert('Error', 'Only available for score plot.', 'error');
        }
    });


    $('#animationBn').bind('click keypress', function (event) {
        if (twoDMode) {
            $.messager.alert('Error', 'Only available in 3D mode.', 'error');
            return;
        }
        animationVal = 0;
        /*if(!animationBool){
         animationBool = true
         animationVal = 0;
         }else{
         animationBool = false
         animationVal = 301;
         }*/
    });

    $('#flCheckbox').change(function () {
        var sc;
        if (mainCheckBool) {
            sc = Scatter.scene();
        } else {
            sc = scene2;
        }

        if (this.checked) {
            initPlane(sc);
        } else {
            turnOffFloor(true);
        }

    });

    $('#updateOpacity').bind('click keypress', function (event) {
        var val = parseFloat($('#edgeSliderOpa').val());
        edgeOpacity = val;
        Scatter.graphData().links.forEach(function (l) {
            l.opacity = val;
            opaLineObj(l, val);
        });

        $('#edgeopacitydlg').dialog('close');
    });

    $('input[type=range]').on('input', function () {
        $(this).trigger('change');
        document.getElementById(this.id + "Text").value = this.value;
    });

    $('#updateNodeOpacity').bind('click keypress', function (event) {
        var val = parseFloat($('#nodeSliderOpa').val());
        nodeOpacity = val;
        Scatter.graphData().nodes.forEach(function (n) {
            opaNodeObj(n, val);
        });

        $('#nodeopacitydlg').dialog('close');
    });


    $('#sdlCheckbox').change(function () {
        var checkBox = document.getElementById("sdlCheckbox");
        if (checkBox.checked === true) {
            turnOffShadow(false);
        } else {
            turnOffShadow(true);
        }
    });


    $('#wlCheckbox').change(function () {
        var sc;
        if (mainCheckBool) {
            sc = Scatter.scene();
        } else {
            sc = scene2;
        }

        if (this.checked) {
            initWall(sc);
        } else {
            turnOffWall(true);
        }

    });

    $("#customBg").spectrum({
        color: "#fff",
        //flat:true,
        showInitial: true,
        cancelText: "", // not shown, as this is not working in flat mode        
        change: function (color) {
            var col = color.toHexString();
            changeBackground(col, col);
        },
        showInput: true,
        showPalette: true,
        showSelectionPalette: true, // true by default
        palette: [
            ['white', '#905356'],
            ['#38597a', 'black']
        ]
    });


    $("#customBgg").spectrum({
        color: "#fff",
        flat: true,
        showInitial: true,
        cancelText: "", // not shown, as this is not working in flat mode        
        change: function (color) {
            var col = color.toHexString();
            var type = $("#bgColType").val();
            if (type === "gradient_dark") {
                changeBackground("#222222", col);
            } else if (type === "gradient_light") {
                changeBackground("#d3d3d3", col);
            } else if (type === "gradient_dark_bottom") {
                changeBackground(col, "#222222");
            } else if (type === "gradient_light_bottom") {
                changeBackground(col, "#d3d3d3");
            } else {
                changeBackground(col, col);
            }

            $('#bgcolordlg').dialog('close');
        },
        showInput: true,
        showPalette: true,
        showSelectionPalette: true, // true by default
        palette: [
            ['white', '#905356'],
            ['#38597a', 'black']
        ]
    });


    $("#customBiplotArrow").spectrum({
        color: "#ff0000",
        //flat:true,
        showInitial: true,
        cancelText: "", // not shown, as this is not working in flat mode        
        change: function (color) {
            var col = color.toHexString();
            arrowColorU = col;
        },
        showInput: true,
        showPalette: true,
        showSelectionPalette: true, // true by default
        palette: [
            ['white', '#905356'],
            ['#38597a', 'black']
        ]
    });

    $("#customBiplotArrow2").spectrum({
        color: "#ff0000",
        //flat:true,
        showInitial: true,
        cancelText: "", // not shown, as this is not working in flat mode        
        change: function (color) {
            var col = color.toHexString();
            arrowColorU = col;
        },
        showInput: true,
        showPalette: true,
        showSelectionPalette: true, // true by default
        palette: [
            ['white', '#905356'],
            ['#38597a', 'black']
        ]
    });


    $("#customBiplotText").spectrum({
        color: "#ff0000",
        //flat:true,
        showInitial: true,
        cancelText: "", // not shown, as this is not working in flat mode        
        change: function (color) {
            var col = color.toHexString();
            biplotTextColorU = col;
        },
        showInput: true,
        showPalette: true,
        showSelectionPalette: true, // true by default
        palette: [
            ['white', '#905356'],
            ['#38597a', 'black']
        ]
    });

    $("#customModule").spectrum({
        color: current_point_color,
        flat: false,
        showInitial: true,
        cancelText: "", // not shown, as this is not working in flat mode        
        change: function (color) {
            var col = color.toHexString();
            current_point_color = col;
        },
        showInput: true,
        showPalette: true,
        showSelectionPalette: true, // true by default
        palette: [
            ['white', '#905356'],
            ['#38597a', 'black']
        ]
    });

    $("#customHalo").spectrum({
        color: current_halo_color,
        flat: false,
        showInitial: true,
        cancelText: "", // not shown, as this is not working in flat mode        
        change: function (color) {
            var col = color.toHexString();
            current_halo_color = col;
        },
        showInput: true,
        showPalette: true,
        showSelectionPalette: true, // true by default
        palette: [
            ['white', '#905356'],
            ['#38597a', 'black']
        ]
    });

    $("#customEncasing").spectrum({
        color: current_encasing_color,
        flat: false,
        showInitial: true,
        cancelText: "", // not shown, as this is not working in flat mode        
        change: function (color) {
            var col = color.toHexString();
            current_encasing_color = col;
        },
        showInput: true,
        showPalette: true,
        showSelectionPalette: true, // true by default
        palette: [
            ['white', '#905356'],
            ['#38597a', 'black']
        ]
    });

    $("#customEdge").spectrum({
        color: bgColor,
        showInitial: true,
        flat: true,
        change: function (color) {

            var col = color.toHexString().replace("#", "0x");
            edgeColor = col;
            var colorValue = parseInt(col.replace("#", "0x"), 16);
            var colored = new Color(colorValue);

            Scatter.graphData().links.forEach(function (l) {
                colorLineObj(l, edgeColor);
            });

            $('#edgecoldlg').dialog('close');
        },
        showInput: true,
        showPalette: true,
        showSelectionPalette: true, // true by default
        palette: [
            ['black', 'white'],
            ['#666666', '#0080ff']
        ]
    });

    $("#customLabel").spectrum({
        flat: false,
        color: bgColor,
        showInitial: true,
        cancelText: "", // not shown, as this is not working in flat mode
        change: function (color) {

            var type = $("#axisOpt").val();
            if (mainCheckBool) {
                labelColor = color.toRgbString();
                for (var propertyNm in spriteysCanvas) {
                    spriteysCanvas[propertyNm].color = labelColor;
                }
                for (var propertyNm in labelArr) {
                    labelArr[propertyNm].color = labelColor;
                }
                labelContainerElem.style.color = labelColor;
                generateAxisLabel(Scatter.scene());
            } else {
                labelColor2 = color.toRgbString();
                generateAxisLabel(scene2);
            }
            $('#nodelabeldlg').dialog('close');
        },
        showInput: true,
        showPalette: true,
        showSelectionPalette: true, // true by default
        palette: [
            ['black', 'white'],
            ['#666666', '#0080ff']
        ]
    });
}

function showLoad() {
    var r = $.Deferred();
    $('#loader').show();
    return r;
}

function updateTextInputSize(val) {
    document.getElementById('nsize').value = val;
}

function updateTextInputOpa(val) {
    document.getElementById('textInputOpa').value = val;
}

function updateTextInputOpaNode(val) {
    document.getElementById('textInputOpaNode').value = val;
}

function updateTextInputWidth(val) {
    document.getElementById('textInputWidth').value = val;
}

function updateTextInputBiplot(val) {
    document.getElementById('textInputBiplot').value = val;
}

function updateTextInput(val, id) {
    document.getElementById(id).value = val;
}

function closest(arr, closestTo) {
    var closest = Math.max.apply(null, arr); //Get the highest number in arr in case it match nothing.
    for (var i = 0; i < arr.length; i++) { //Loop the array
        if (arr[i] >= closestTo && arr[i] < closest)
            closest = arr[i]; //Check if it's higher than your number, but lower than your closest value
    }
    return closest; // return the value
}

function searchLoadingTable() {
    var search = $('#nodeid').val();
    if (search === "") {
        resetNetwork();
        return;
    }
    var hitrow = "NA";
    var current_row;
    for (var i = 0; i < node_rows_csize.length; i++) {
        current_row = node_rows_csize[i];
        if (current_row.ID === search | current_row.Label === search) {
            hitrow = current_row;
            var page_num = Math.ceil(i / 30);
            var row_num = i % 30;
            $('#dg').datagrid('gotoPage', page_num);
            $('#dg').datagrid('loadData', node_rows_csize);
            $('#dg').datagrid('selectRow', row_num);
            break;
        }
    }
    if (hitrow === "NA") {
        $.messager.alert('', "Could not find the given node: " + search, 'error');
        return;
    }
}

function resetNetwork() {
    bgColor = "#ffffff";
    labelColOpt = "#222222";
    Scatter.backgroundColor(bgColor);

    Scatter.cameraPosition({
        x: initRadius * 1.2,
        y: initRadius * 1.2,
        z: initRadius * 1.7
    }, // new position
            {
                x: 0,
                y: 0,
                z: 0
            }, // lookAt ({ x, y, z })
            2000 // ms transition duration
            );
    Scatter.controls().target.set(0, 0, 0);

    Scatter.graphData().nodes.forEach(function (n) {
        n.highlight = 0;
        colorNodeObj(n, n.tcolor);
        opaNodeObj(n, 0.8);
        if (n.meta !== "mcia.seg") {
            sizeNodeObj(n, 1);
        }
        deleteOutline(n);
    });

    Scatter.graphData().links.forEach(function (l) {
        l.highlight = 0;
        l.width = 0;
        colorLineObj2(l, edgeColor);
        sizeLineObj(l, 0);
    });

    Scatter.scene().traverse(function (child) {
        if (child instanceof Mesh && typeof child.userData === "string") {
            if (child.userData.includes("fatline")) {
                child.visible = false;
            }
        }
    });
    if (gData.navigation !== "NA") {
        scene2.traverse(function (child) {
            if (child instanceof Mesh) {
                if (child.userData.nodeData !== undefined) {
                    var colorValue = parseInt(child.userData.nodeData.tcolor.replace("#", "0x"), 16);
                    var col = new Color(colorValue);
                    child.userData.nodeData.highlight = 0;
                    child.material.color = col;
                    child.material.opacity = 0.8;

                }
            }
        });
    }
}

function rescale2Range(inputY, yMax, yMin, xMax, xMin) {
    var percent = (inputY - yMin) / (yMax - yMin);
    var outputX = percent * (xMax - xMin) + xMin;
    return outputX;
}

function materialNodeObj(n, mat) {
    const object = n.__threeObj;
    if (object !== null && object !== undefined) {
        const oldMaterial = JSON.parse(JSON.stringify(object.material))
        if (mat === "MeshPhysicalMaterial") {
            object.material = new MeshPhysicalMaterial({
                opacity: 1,
                color: oldMaterial.color,
                metalness: 0.1,
                roughness: 0.1,
                transparent: true,
                depthWrite: true
            });
        }
    }
}

function sizeNodeObj(n, size) {
    //n.size = size;
    const object = n.__threeObj;
    if (object !== null && object !== undefined) {
        object.userData.scaling = [size, size, size];
        object.scale.x = size;
        object.scale.y = size;
        object.scale.z = size;
    }
}

function colorNodeObj(n, color) {
    var id = n.id;
    n.color = color.replace("0x", "#");
    var colorValue = parseInt(color.replace("#", "0x"), 16);
    var col = new Color(colorValue);
    const object = n.__threeObj;
    if (object !== null && object !== undefined) {
        const clonedMaterial = object.material.clone();
        clonedMaterial.setValues({
            color: col
        });
        object.material = clonedMaterial;
    }
}

function shapeNodeObj(n, type) {

    const object = n.__threeObj;
    if (object.userData.type !== type) {
        if (object !== null && object !== undefined) {
            if (object.geometry !== undefined) {
                object.geometry.dispose();
                object.geometry = undefined;
            }
            if (type === "cone") {
                var newgeom = new ConeGeometry(20, 40);
                object.geometry = newgeom;
            } else if (type === "triangle") {
                var newgeom = new ConeGeometry(20, 40, 4);
                object.geometry = newgeom;
            } else if (type === "square") {
                var newgeom = new BoxGeometry(32, 15, 15);
                object.geometry = newgeom;
            } else if (type === "cylinder") {
                var newgeom = new CylinderGeometry(22, 12, 18);
                object.geometry = newgeom;
            } else if (type === "ring") {
                var newgeom = new TorusGeometry(20, 3, 16, 30);
                object.geometry = newgeom;
            } else {
                var newgeom = new SphereGeometry(20, 8, 8);
                object.geometry = newgeom;
            }
        }
        object.userData.type = type;
    }
}

function opaNodeObj(n, op) {
    const object = n.__threeObj;
    if (object !== null && object !== undefined) {
        const clonedMaterial = object.material.clone();
        if (op !== 1) {
            clonedMaterial.setValues({
                opacity: op,
                transparent: true
            });
        } else if (op === 0) {
            clonedMaterial.setValues({
                visible: false
            });
        } else {
            clonedMaterial.setValues({
                opacity: op,
                transparent: false
            });
        }
        object.material = clonedMaterial;
    }
}

function opaNodeObjForLabelToggle(n, op) {
    const object = n.__threeObj;
    console.log(n)
    if (object !== null && object !== undefined) {
        const clonedMaterial = object.material.clone();
        if (op !== 1) {
            clonedMaterial.setValues({
                opacity: op,
                transparent: true,
                depthWrite: false
            });
        } else {
            clonedMaterial.setValues({
                opacity: op,
                transparent: false,
                depthWrite: true
            });
        }
        object.material = clonedMaterial;
    }
}

function colorLineObj(l, color) {
    var id = l.id;
    l.color = color.replace("0x", "#");
    var colorValue = parseInt(color.replace("#", "0x"), 16);
    var col = new Color(colorValue);
    const object = l.__lineObj;
    if (object !== undefined && object !== null) {
        const clonedMaterial = object.material.clone();
        clonedMaterial.setValues({
            color: col
        });

        object.material = clonedMaterial;
    }
}

function opaLineObj(l, opacity) {
    var id = l.id;
    const object = l.__lineObj;
    if (object !== undefined && object !== null) {
        const clonedMaterial = object.material.clone();
        if (opacity === 0) {
            clonedMaterial.setValues({
                visible: false
            });
            object.material = clonedMaterial;
        } else {
            clonedMaterial.setValues({
                opacity: opacity,
                visible: true
            });
            object.material = clonedMaterial;
        }
    }
}

function sizeLineObj(l, size) {
    var id = l.id;
    const object = l.__lineObj;
    if (object !== undefined && object !== null) {
        const clonedMaterial = object.material.clone();
        if (size === 0) {
            object.visible = true;


        } else {
            object.visible = false;
            var geometry = new LineGeometry();
            var pos = [l.source.fx, l.source.fy, l.source.fz, l.target.fx, l.target.fy, l.target.fz];


            geometry.setPositions(pos);
            //geometry.setColors(colors);
            var colorValue1 = parseInt(highlightColor.replace("#", "0x"), 16);
            var col1 = new Color(colorValue1);

            var matLine = new LineMaterial({
                color: col1,
                linewidth: size / 1000, // in pixels
                //vertexColors: VertexColors,
                //resolution:  // to be set by renderer, eventually
                dashed: false

            });

            var line = new Line2(geometry, matLine);
            line.computeLineDistances();
            line.scale.set(1, 1, 1);
            lineObj[l.id] = line;
            line.userData = "fatline" + l.id;
            Scatter.scene().add(lineObj[l.id]);
            //Scatter.scene().remove(lineObj[l.id]);
        }
    }
}


function colorLineObj2(l, type) { // multiple color
    const object = l.__lineObj;
    if (parseFloat(l.opacity) === 0) {
        //colorLineObj(l, type)
        return;
    }
    if (object.geometry !== null && object.geometry !== undefined) {
        var count = 2;
        object.geometry.addAttribute('color', new BufferAttribute(new Float32Array(count * 3), 3));

        var colorValue1 = parseInt(l.source.color.replace("#", "0x"), 16);

        var col1 = new Color(colorValue1);
        var colorValue2 = parseInt(l.target.color.replace("#", "0x"), 16);
        var col2 = new Color(colorValue2);
        var i = 0;
        object.geometry.attributes.color.array[i] = col1["r"];
        object.geometry.attributes.color.array[i + 1] = col1["g"];
        object.geometry.attributes.color.array[i + 2] = col1["b"];
        object.geometry.attributes.color.array[i + 3] = col2["r"];
        object.geometry.attributes.color.array[i + 4] = col2["g"];
        object.geometry.attributes.color.array[i + 5] = col2["b"];

        object.material.color = col1
        //object.material.vertexColors = VertexColors;
        //object.geometry.dispose();
        //object.geometry = undefined;
        if (type === "cone") {
            //var newgeom =  new ConeGeometry(10, 20);
            //object.geometry = newgeom;
        }
    }

    opaLineObj(l, parseFloat(l.opacity));
}


function colorAdapt(col) {
    var colorValue = parseInt(col.replace("#", "0x"), 16);
    var color = new Color(colorValue);
    return color;
}


function iterate(obj, func) {
    for (var k in obj) {
        if (!obj.hasOwnProperty(k)) {
            continue;
        }

        func(obj[k], k);
    }
}



function hexToRgb(hex) {
    // Expand shorthand form (e.g. "03F") to full form (e.g. "0033FF") 
    var shorthandRegex = /^#?([a-f\d])([a-f\d])([a-f\d])$/i;
    hex = hex.replace(shorthandRegex, function (m, r, g, b) {
        return r + r + g + g + b + b;
    });

    var result = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i.exec(hex);
    return result ? [
        parseInt(result[1], 16),
        parseInt(result[2], 16),
        parseInt(result[3], 16)
    ] : null;
}

function determinePercentile(arr, p) {
    if (arr.length === 0)
        return 0;
    if (typeof p !== 'number')
        throw new TypeError('p must be a number');
    if (p <= 0)
        return arr[0];
    if (p >= 1)
        return arr[arr.length - 1];

    var index = arr.length * p,
            lower = Math.floor(index),
            upper = lower + 1,
            weight = index % 1;

    if (upper >= arr.length)
        return arr[lower];
    return arr[lower] * (1 - weight) + arr[upper] * weight;
}

function hex(c) {
    var s = "0123456789abcdef";
    var i = parseInt(c);
    if (i === 0 || isNaN(c))
        return "00";
    i = Math.round(Math.min(Math.max(0, i), 255));
    return s.charAt((i - i % 16) / 16) + s.charAt(i % 16);
}

/* Convert an RGB triplet to a hex string */
function convertToHex(rgb) {
    return hex(rgb[0]) + hex(rgb[1]) + hex(rgb[2]);
}

/* Remove '#' in color hex string */
function trim(s) {
    return (s.charAt(0) === '#') ? s.substring(1, 7) : s;
}

/* Convert a hex string to an RGB triplet */
function convertToRGB(hex) {
    var color = [];
    color[0] = parseInt((trim(hex)).substring(0, 2), 16);
    color[1] = parseInt((trim(hex)).substring(2, 4), 16);
    color[2] = parseInt((trim(hex)).substring(4, 6), 16);
    return color;
}


function getContrastColor(hexcolor) {
    hexcolor = hexcolor.replace("#", "");
    var r = parseInt(hexcolor.substr(0, 2), 16);
    var g = parseInt(hexcolor.substr(2, 2), 16);
    var b = parseInt(hexcolor.substr(4, 2), 16);
    var yiq = ((r * 299) + (g * 587) + (b * 114)) / 1000;
    return (yiq >= 128) ? '#222222' : '#ffffff';
}

function removeOptions(selectbox) {
    if (selectbox !== undefined && selectbox !== null) {
        var i;
        for (i = selectbox.options.length - 1; i >= 0; i--) {
            selectbox.remove(i);
        }
    }
}

function generateSprite(color) {
    var canvas = document.createElement('canvas');
    canvas.width = 256;
    canvas.height = 256;
    var context = canvas.getContext('2d');
    var gradient = context.createRadialGradient(canvas.width / 2, canvas.height / 2, 0, canvas.width / 2, canvas.height / 2, canvas.width / 2);
    gradient.addColorStop(0, 'rgba(0,0,0,0)');
    gradient.addColorStop(0.2, 'rgba(120,120,120,0)');
    gradient.addColorStop(0.5, color.replace("0x", "#"));
    gradient.addColorStop(1, 'rgba(0,0,0,0)');
    context.fillStyle = gradient;
    context.fillRect(0, 0, canvas.width, canvas.height);
    return canvas;
}


function outlineNode(n, color) {
    var nsize;
    if (n.outline === 1) {
        deleteOutline(n);
    }
    n.outline = 1;
    var colorValue = parseInt(n.color.replace("#", "0x"), 16);
    var particle = new Sprite(new SpriteMaterial({
        map: new CanvasTexture(generateSprite(color)),
        alphaTest: 0.5,
        transparent: true,
        fog: true
    }));
    particle.scale.x = particle.scale.y = n.__threeObj.userData.scaling[0] * 3 + 70;
    var jsonKey = n.id;
    outlineUIs[jsonKey] = particle;
    outlineUIs[jsonKey].userData.type = "halo";
    outlineUIs[jsonKey].userData.id = n.id;
    outlineUIs[jsonKey].userData.color = color.replace("#", "0x");
    //particle.position.set(threeNodes[n.id].position.x, threeNodes[n.id].position.y, threeNodes[n.id].position.z)
    var obj = n.__threeObj;

    obj.add(particle);
}




function highlightNode(node) {
    node.highlight = 1;
    if (highlightType === "mixed") {
        colorNodeObj(node, highlightColor);
        //sizeNodeObj(node, node.size+2)
        //node.size = node.size + 2;
        return;
    } else if (highlightType === "node") {
        colorNodeObj(node, highlightColor);
        //sizeNodeObj(node, node.size+2)
        //node.size = node.size + 2;
    } else {
        if (outlineUIs[node.id] === null || outlineUIs[node.id] === undefined) {
            outlineNode(node, highlightColor);
        } else {
            deleteOutline(node);
            outlineNode(node, highlightColor);
        }
    }
}

function highlightNodeAndNeighbours(node) {
    node.highlight = 1;
    var links = Scatter.graphData().links;
    var nodes = Scatter.graphData().nodes;
    if (["mcia"].indexOf(gData.reductionOpt) !== -1) {
        var int_node_id = node.id.substring(0, node.id.length - 2);
        var idsArr = [node.id, int_node_id];
        var node_arr = [node];
        for (var i = 0; i < links.length; i++) {
            var link = links[i];
            if (idsArr.indexOf(link.source.id) !== -1) {
                node_arr.push(link.target);
            } else if (idsArr.indexOf(link.target.id) !== -1) {
                node_arr.push(link.source);
            }
        }
        for (var i = 0; i < nodes.length; i++) {
            if (nodes[i].id === int_node_id) {
                node_arr.push(nodes[i]);
            }
        }
    } else {
        var node_arr = [node];
        for (var i = 0; i < links.length; i++) {
            var link = links[i];
            if (link.source.id === node.id) {
                node_arr.push(link.target);
            } else if (link.target.id === node.id) {
                node_arr.push(link.source);
            }
        }
    }

    for (var i = 0; i < node_arr.length; i++) {
        var n = node_arr[i];
        if (highlightType === "mixed" || highlightType === "node") {
            outlineNode(n, highlightColor);
            n.highlight = 1;
            //sizeNodeObj(node, node.size+2)
            //node.size = node.size + 2;
        } else {
            if (outlineUIs[n.id] === null || outlineUIs[n.id] === undefined) {
                outlineNode(n, highlightColor);
                n.highlight = 1;
            } else {
                deleteOutline(node);
                outlineNode(n, highlightColor);
                n.highlight = 1;
            }
        }
    }

    displayCurrentSelectedNodes(node_arr, "");
}


function clearSelection() {
    Scatter.graphData().nodes.forEach(function (n) {
        if (n.highlight === 1) {
            n.color = n.tcolor;
            unhighlightNode(n);
            highlightNode(n);
        }
    });
}

function unhighlightNode(node) {
    node.highlight = 0;
    if (highlightType === "mixed") {

    } else if (highlightType === "node") {
        const object = node.__threeObj;
        if (node.prevCol === undefined || node.prevCol === null) {
            colorNodeObj(node.id, node.tcolor);
        } else {
            colorNodeObj(node.id, node.prevCol);
        }
    }

    if (outlineUIs[node.id] !== null || outlineUIs[node.id] !== undefined) {
        deleteOutline(node);
    }
}

function updateTextInputLabel(val) {
    document.getElementById('textInputLabelThreshold').value = val;
}

function shiftCameraToNode(node) {
    const distance = 300;
    const distRatio = 1 + distance / Math.hypot(node.fx, node.fy, node.fz);

    Scatter.cameraPosition({
        x: node.fx * distRatio,
        y: node.fy * distRatio,
        z: node.fz * distRatio
    }, // new position
            {
                x: 0,
                y: 0,
                z: 0
            }, // lookAt ({ x, y, z })
            3000 // ms transition duration
            );

    setTimeout(function () {
        Scatter.controls().target.set(node.fx, node.fy, node.fz);
        currentOrigin = v(node.fx, node.fy, node.fz);
    }, 3200);

}



function percentile(arr, p) {
    if (arr.length === 0)
        return 0;
    if (typeof p !== 'number')
        throw new TypeError('p must be a number');
    if (p <= 0)
        return arr[0];
    if (p >= 1)
        return arr[arr.length - 1];

    arr.sort(function (a, b) {
        return a - b;
    });
    var index = (arr.length - 1) * p;
    lower = Math.floor(index),
            upper = lower + 1,
            weight = index % 1;

    if (upper >= arr.length)
        return arr[lower];
    return arr[lower] * (1 - weight) + arr[upper] * weight;
}

function initTable() {

    //$('#dg').datagrid('clearSelections');
    var data_grid = $('#dg');
    data_grid.datagrid('loadData', []);
    data_grid.datagrid('fitColumns', true);
}

function deleteOutline(n) {
    if (outlineUIs[n.id] !== undefined) {
        var group = n.__threeObj;
        for (var i = 0; i < group.children.length; i++) {
            var child = group.children[i];
            if (child.userData.type === "halo") {
                group.remove(child);
            }
        }

        //group.remove(group.children[group.children.length - 1]);
        var mesh = outlineUIs[n.id];
        Scatter.scene().remove(mesh);
        if (mesh) {
            if (mesh.material.geometry) {
                mesh.geometry.ZY();
                mesh.geometry = undefined;
            }
            if (mesh.material.map)
                mesh.material.map.ZY();
            if (mesh.material.lightMap)
                mesh.material.lightMap.ZY();
            if (mesh.material.bumpMap)
                mesh.material.bumpMap.ZY();
            if (mesh.material.normalMap)
                mesh.material.normalMap.ZY();
            if (mesh.material.specularMap)
                mesh.material.specularMap.ZY();
            if (mesh.material.envMap)
                mesh.material.envMap.ZY();
            mesh.material.ZY();
            mesh.material = undefined;
            mesh = undefined;
            outlineUIs[n.id] = null;
            delete outlineUIs[n.id];
        }
    }
}

function addLabel(node) {
    if (node.labeled === 2) {
        return;
    }
    var myText = new SpriteText(node.label);
    myText.color = labelColOpt;
    myText.visible = true;
    myText.position.y = node.size / 2 + 15;
    myText.textHeight = 12;
    myText.fontSize = 100;
    myText.depthWrite = false;
    myText.userData = {};
    myText.userData.id = node.label;


    const elem = document.createElement('div');
    elem.textContent = node.label.replace("-", "‑");
    labelContainerElem.appendChild(elem);
    node.labeled = 1;

    node.__threeObj.add(myText);

    labelContainerElem.style.color = labelColOpt;
    if (htmlTextBool) {
        elem.style.display = "block";
        myText.visible = false;
    } else {
        elem.style.display = "none";
        myText.visible = true;
    }
    spriteys[node.id] = elem;
    spriteysCanvas[node.id] = myText;
}

function addLabelText(text, size) {
    var myText = new SpriteText(text);
    myText.color = labelColOpt;
    myText.visible = true;
    //myText.position.y = node.size / 2 + 15;
    myText.textHeight = 12;
    myText.fontSize = 100;
    myText.depthWrite = false;
    myText.userData = {};
    myText.userData.id = text;
    myText.scale.x = myText.scale.x * size;
    myText.scale.y = myText.scale.y * size;
    myText.scale.z = myText.scale.z * size;

    const elem = document.createElement('div');
    elem.textContent = text.replace("-", "‑");
    labelContainerElem.appendChild(elem);
    //node.labeled = 1;
    return(myText);
}



function highlightSeed() {
    if (outlineBool === false) {
        if (current_main_plot === "score") {

            scene2.traverse(function (child) {
                if (child instanceof Mesh) {
                    var n = child.userData.nodeData;
                    if (n !== undefined) {
                        if (n.seedArr === "seed") {
                            child.material.opacity = 1;
                            //var colorValue = parseInt(highlightColor.replace("#", "0x"), 16);
                            //var col = new Color(colorValue);
                            //child.material.color = col;

                            child.visible = true;
                        } else {
                            child.material.opacity = 0.2;
                            var colorValue = parseInt("0xd3d3d3", 16);
                            var col = new Color(colorValue);
                            child.material.color = col;

                            child.visible = true;
                        }
                    }
                }
            });
        } else {
            var nodes = Scatter.graphData().nodes;
            nodes.forEach(function (n) {
                if (n.seedArr === "seed") {
                    outlineNode(n, highlightColor);
                }
            });

        }

        $('#loader').hide();
    }
}

function hideSeed() {
    if (outlineBool === true) {
        if (current_main_plot === "score") {

            scene2.traverse(function (child) {
                if (child instanceof Mesh) {
                    var n = child.userData.nodeData;
                    if (n !== undefined) {

                        child.material.opacity = 1;
                        var colorValue = parseInt(n.colorb.replace("#", "0x"), 16);
                        var col = new Color(colorValue);
                        child.material.color = col;

                        child.visible = true;

                    }
                }
            });
        } else {
            var nodes = Scatter.graphData().nodes;
            nodes.forEach(function (n) {
                if (n.seedArr === "seed") {
                    if (outlineUIs[n.id] !== undefined || outlineUIs[n.id] !== null) {
                        deleteOutline(n);
                    }
                }
            });

        }

        $('#loader').hide();
    }
}


function deleteSprite(node) {
    if (!htmlTextBool) {
        if (spriteysCanvas[node.id] !== undefined) {
            spriteysCanvas[node.id].material.ZY();
            spriteysCanvas[node.id].geometry.ZY();
            node.labeled = 0;

            Scatter.scene().remove(spriteysCanvas[node.id]);
            delete spriteysCanvas[node.id];
        }
    } else {

        if (spriteys[node.id] !== undefined) {
            node.labeled = 0;
            var element = spriteys[node.id];
            element.parentNode.removeChild(element);
            spriteys[node.id] = null;
            delete spriteys[node.id];
        }
    }
}

function zoomIn(distance) {
    Scatter.camera().translateZ(-distance);
}

function zoomOut(distance) {
    Scatter.camera().translateZ(distance);
}

function takeScreenshot() {
    var img = new Image();
    var scene = Scatter.scene();
    var camera = Scatter.camera();

    if (gData.navigation !== "NA") {
        takeImageBool = true;
    }
    updateLabelPositions();
    var width = parseInt(Scatter.renderer().domElement.width);
    var height = parseInt(Scatter.renderer().domElement.height);
    if (window.devicePixelRatio > 1) {
        width = width / window.devicePixelRatio;
        height = height / window.devicePixelRatio;
    }
    Scatter.renderer().setViewport(0, 0, width, height);
    Scatter.renderer().setScissor(0, 0, width, height);
    Scatter.renderer().render(scene, camera);
    if (!embeddedCollapsed) {
        Scatter.renderer().setViewport(0, 0, 300 * camera.aspect - 50, 300);
        Scatter.renderer().setScissor(0, 0, 300 * camera.aspect - 50, 300);
        Scatter.renderer().render(scene2, camera);
    }

    img.src = Scatter.renderer().domElement.toDataURL();
    //document.getElementById("downloadimage").src = img.src;
    //document.getElementById("downloadimage").appendChild(img);

    var link = document.createElement('a');
    link.download = 'scatter3D.png';
    link.href = img.src
    link.click();
    //$("#pngdialog").dialog('openCenter');
}

var resizeId;

function onWindowResize() {
    clearTimeout(resizeId);
    resizeId = setTimeout(doneResizing, 500);
    //alert("resize");
    const container = document.getElementById("3d-graph");
    var camera = Scatter.camera();

    function doneResizing() {
        camera.aspect = container.clientWidth / container.clientHeight;
        camera.updateProjectionMatrix();
        Scatter.width(container.clientWidth);
        Scatter.height(container.clientHeight);
        changeBackground("#ffffff", "#ffffff");

        Scatter.camera().position.x = 1500;
        Scatter.camera().position.y = 1300;
        Scatter.camera().position.z = 3200;

    }
}



function addMetaSphere(shownodes, inx) {
    if (boundingClusters[inx] !== undefined) {
        deleteMetaSphere(inx, Scatter.scene());
    }
    var vertices = [];
    shownodes.forEach(function (n) {
        var pos = new Vector3();
        pos.x = n.x;
        pos.y = n.y;
        pos.z = n.z;
        vertices.push(pos);
    });

    var color = highlightColor.replace("0x", "#");
    var colorValue = parseInt(color.replace("#", "0x"), 16);
    var col = new Color(colorValue);

    var sphere = new Sphere().setFromPoints(vertices);
    var sphereGeometry = new SphereGeometry(sphere.radius + 10, 64, 64);
    var sphereMaterial = new MeshPhongMaterial({
        opacity: 0.3,
        color: col,
        transparent: true,
        depthWrite: false
    });

    var sphereMes = new Mesh(sphereGeometry, sphereMaterial);
    sphereMes.position.set(sphere.center.x, sphere.center.y, sphere.center.z);
    boundingClusters[inx] = sphereMes;
    activeModules[inx] = inx;
    Scatter.scene().add(sphereMes);
}

function deleteShade(t) {
    var group = t.__threeObj;
    if (group !== undefined) {
        for (var i = group.children.length - 1; i >= 0; i--) {
            var myMesh = group.children[i];
            myMesh.geometry.ZY();
            myMesh.material.ZY();
            myMesh = null;
            group.remove(group.children[i]);
        }

        group.geometry.ZY();
        group.material.ZY();
        group = null;
        delete t.__threeObj;
    }
    var nobj = t.__threeObj;
    if (nobj !== undefined) {
        for (var i = nobj.children.length - 1; i >= 0; i--) {
            var myMesh = nobj.children[i];
            myMesh.geometry.ZY();
            myMesh.material.ZY();
            myMesh = null;
            t.__threeObj.remove(nobj.children[i]);
        }

        t.__threeObj.geometry.ZY();
        t.__threeObj.material.ZY();
        t.__threeObj = null;
        delete t.__threeObj;
        //Scatter.scene().remove(nobj)
    }
}

function setShade() {
    setTimeout(function () {
        var labelProperty = $("#labelThresholdProperty").val();
        hoveredNode = null;

        Scatter.graphData().nodes.forEach(function (n) {
            deleteSprite(n);
        });
        spriteys = null;
        spriteys = {};

        var nodes_num = Scatter.graphData().nodes.length;
        for (var i = 0; i < nodes_num; i++) {
            var node = Scatter.graphData().nodes[i];
            // add text sprite as child
            if (labelMode === "global") {
                if (node.attributes !== undefined) {
                    if (nodes_num > 50) {
                        //addLabel(node)
                    } else {
                        //addLabel(node)
                    }
                }
            }
            if (node.type === "metabolite") {
                scatterType = "multi";
                node.tcolor = highlightColor;
                node.color = highlightColor;
                colorNodeObj(node, highlightColor);
            } else if (node.type === "microbe") {
                node.tcolor = "#ff0000";
                node.color = "#ff0000";
                colorNodeObj(node, "#ff0000");
            }
        }
        var deg_arr = {};
        var deg_threshold = {};

        if (labelMode === "module") {
            for (var i = 0; i < sub_modules.length; i++) {
                deg_arr[i] = [];
                Scatter.graphData().nodes.forEach(function (n) {
                    if (n.module === i) {
                        deg_arr[i].push(n[labelProperty]);
                    } else {

                    }
                });
                //if(deg_top25 === 0){
                var deg_top25 = Math.max(...deg_arr[i]);
                //}
                deg_threshold[i] = deg_top25;
            }

            Scatter.graphData().nodes.forEach(function (n) {
                if (parseInt(n[labelProperty]) >= parseInt(deg_threshold[n.module])) {
                    addLabel(n);
                } else if (n.attributes !== undefined && n.module === undefined) {

                    if (n[labelProperty] > labeling_threshold && spriteys[n.id] === undefined) {
                        idsArr.push(n.id);
                        addLabel(n);
                    }
                }
            });


        }
        var links_num = Scatter.graphData().links.length;

        for (var i = 0; i < links_num; i++) {
            var l = Scatter.graphData().links[i];
            if (l.label !== undefined) {
                // addLabelLink(l);
            }
            if (!spherical && !bundled) {
                opaLineObj(l, l.opacity);
            }
        }

    }, 1000);
}

function LightenDarkenColor(col, amt) {

    var usePound = false;

    if (col[0] === "#") {
        col = col.slice(1);
        usePound = true;
    }

    var num = parseInt(col, 16);

    var r = (num >> 16) + amt;

    if (r > 255)
        r = 255;
    else if (r < 0)
        r = 0;

    var b = ((num >> 8) & 0x00FF) + amt;

    if (b > 255)
        b = 255;
    else if (b < 0)
        b = 0;

    var g = (num & 0x0000FF) + amt;

    if (g > 255)
        g = 255;
    else if (g < 0)
        g = 0;

    return (usePound ? "#" : "") + (g | (b << 8) | (r << 16)).toString(16);

}

function setHighOpt() {
    var val = $('#highOpt').val();
    if (val === "mixed") {
        highlightType = "mixed";
    }
    if (val === "color") {
        highlightType = "node";
    } else if (val === "halo") {
        highlightType = "halo";
    } else if (val === "unh") {
        Scatter.graphData().nodes.forEach(function (n) {
            if (n.prevCol === undefined || n.prevCol === null) {
                colorNodeObj(n, n.tcolor);
            } else {
                colorNodeObj(n, n.prevCol);
            }
            //n.color = n.tcolor;
            if (outlineUIs[n.id] !== null || outlineUIs[n.id] !== undefined) {
                deleteOutline(n);
            }
            n.highlight = 0;
        });
    } else {
        return;
    }
}


function downloadJson(obj) {

    var contentType = "application/json;charset=utf-8;";
    var fileLnk = $("#fileLnk");
    var file = new Blob([obj], {
        type: contentType
    });
    fileLnk.empty();
    fileLnk.append("Right click the link below, then 'Save Link As ... ' to download the file<br/><br/>");

    let dataStr = JSON.stringify(obj);
    let dataUri = 'data:application/json;charset=utf-8,' + encodeURIComponent(dataStr);
    let exportFileDefaultName = 'omicsanalyst.json';

    let linkElement = document.createElement('a');
    linkElement.setAttribute('href', dataUri);
    linkElement.setAttribute('download', exportFileDefaultName);
    linkElement.innerHTML = '<u>' + exportFileDefaultName + '</u>';

    fileLnk.append(linkElement);
    //$.messager.progress('close');
    $("#filedialog").dialog('open');
}


function changeBackground(color1, color2) {

    var col = color1.replace("0x", "#");
    bgColor = color1;
    if (!mainCheckBool) {
        bgColor2 = color1;
        var colorValue = parseInt(color1.replace("#", "0x"), 16);
        var col = new Color(colorValue);
        axisColorU = getContrastColor(color1.replace("#", ""));
        generateAxisLabel(scene2);

    } else {
        var type = $("#axisOpt").val();
        bgColor = color1;

        var labelCol = getContrastColor(color1.replace("#", ""));
        if (labelCol !== labelColOpt) {
            labelColOpt = labelCol;
            labelContainerElem.style.color = labelColOpt;
            labelColSpectrum.setSpectrum(color1, labelCol);

            for (var propertyNm in spriteysCanvas) {
                if (spriteysCanvas[propertyNm] !== undefined)
                    spriteysCanvas[propertyNm].color = labelColOpt;
            }

        }
        axisColorU = getContrastColor(color1.replace("#", ""));
        generateAxisLabel(Scatter.scene());

    }
    for (const propertyName in myTexts) {
        if (mainCheckBool) {
            if (propertyName.includes(Scatter.scene().uuid)) {
                myTexts[propertyName].color = axisColorU;
            }
        } else {
            if (propertyName.includes(scene2.uuid)) {
                myTexts[propertyName].color = axisColorU;
            }
        }
    }

    const collection = document.getElementsByClassName("scene-tooltip");
    collection[0].style.color = axisColorU;
    if (mainCheckBool) {
        setGradientColor(color1, color2);
        gradCol1 = color1;
        gradCol2 = color2;
        legendUtils.updateTextColor(axisColorU);
    } else {
        scene2.background = new Color(col);
    }


}


function updateMetaSphere() {
    var propertyNms = Object.keys(boundingClusters);
    for (var i = 0; i < propertyNms.length; i++) {
        var shownodes = [];

        Scatter.graphData().nodes.forEach(function (n) {
            if (n.module + "" === propertyNms[i]) {
                shownodes.push(n);
            }
        });
        var vertices = [];
        shownodes.forEach(function (n) {
            var pos = new Vector3();
            pos.x = n.x;
            pos.y = n.y;
            pos.z = n.z;
            vertices.push(pos);
        });
        var sphere = new Sphere().setFromPoints(vertices);
        var sphe = boundingClusters[propertyNms[i]];
        sphe.position.set(sphere.center.x, sphere.center.y, sphere.center.z);
        //var sphe1 = boundingClustersMesh[propertyNms[i]];
        //sphe1.position.set(sphere.center.x, sphere.center.y, sphere.center.z)
    }
}

function addMetaSphereExperimental(shownodes, inx) {
    if (boundingClusters[inx] !== undefined) {
        deleteMetaSphere(inx, Scatter.scene());
    }
    var shownodesids = [];
    shownodes.forEach(function (n) {
        shownodesids.push(n.id);
    });


    Scatter.graphData().links.forEach(function (l) {

        if (shownodesids.indexOf(l.source.id) !== -1 || shownodesids.indexOf(l.target.id) !== -1) {
            l.module = 1;
        }

        if (l.source.module !== l.target.module) {
            l.module = 0;
        }
    });

    Scatter.graphData().nodes.forEach(function (n) {
        if (shownodesids.indexOf(n.id) !== -1) {
            sizeNodeObj(n, n.tsize / 1.5);
            n.moduleNumber = inx;
            //colorNodeObj(n, highlightColor)
        }
    });

}

function startAnimation() {
    radius = Scatter.camera().position.distanceTo(Scatter.scene().position);
    animationVal = 0;
    takeVideo = true;
    $('#animationdlg').dialog('close');
    $('#loader').show();
    //startRecording();
}

function computeSceneRadius() {
    var nodes = Scatter.graphData().nodes;
    var scene = Scatter.scene();
    var radius = 0;
    nodes.forEach(function (n) {
        var distance = distanceVector(n, scene.position);
        radius = Math.max(distance, radius);
    });
    return (radius);
}

function initNodeSize() {
    if (document.getElementById('minclust') !== null) {
        document.getElementById('nsize').value = 2;
        document.getElementById('minclust').value = 1;
        document.getElementById('nodeSliderSize').value = 2;
    }
}

function zoomCheck() { //zoom results in label showin up  
    if (hideLabel || labelNodeMode) {
        return;
    }

    var camera = Scatter.camera();
    camera.updateMatrix();
    camera.updateMatrixWorld();
    var frustum = new Frustum();
    frustum.setFromMatrix(new Matrix4().multiplyMatrices(camera.projectionMatrix, camera.matrixWorldInverse));
    var vFOV = camera.fov * Math.PI / 180;
    // Your 3d point to check  

    //var pos = new Vector3(x, y, z);  

    if (Scatter.graphData().nodes.length < 500) {
        Scatter.graphData().nodes.forEach(function (n) {

            var coord = {
                x: n.fx,
                y: n.fy,
                z: n.fz
            };
            var coordv = new Vector3(n.fx, n.fy, n.fz);

            if (frustum.containsPoint(coord)) {

                var dist = distanceVector(coord, camera.position); // convert vertical fov to radians  

                var height = 2 * Math.tan(vFOV / 2) * dist;
                var fraction = n.size * 15 / height;
                if (!fixedLabels) {

                    if (fraction > 0.40 && spriteysCanvas[n.id] === undefined && n.labeled !== 1) {
                        if (n.labeled === 2) {
                            spriteysCanvas[n.id].material.visible = true;
                        } else {
                            if (!htmlTextBool) {
                                addLabel(n);
                            }
                            n.labeled = 2;
                        }
                    } else if (fraction > 0.4 && spriteysCanvas[n.id] !== undefined && n.labeled !== 1) {
                        spriteysCanvas[n.id].material.visible = true;

                        n.labeled = 2;
                    } else if (fraction < 0.38 && spriteysCanvas[n.id] !== undefined && n.labeled !== 1) {
                        spriteysCanvas[n.id].material.visible = false;

                        n.labeled = -1;
                    }
                }
            }
        });
    } else {
        var nodeDists = {};
        Scatter.graphData().nodes.forEach(function (n) {
            var coord = {
                x: n.fx,
                y: n.fy,
                z: n.fz
            };
            var coordv = new Vector3(n.fx, n.fy, n.fz);

            if (frustum.containsPoint(coord)) {
                var dist = distanceVector(coord, camera.position);
                nodeDists[n.id] = dist;
            }
        });

        var dists = Object.keys(nodeDists).map(function (key) {
            return {
                key: key,
                value: this[key]
            };
        }, nodeDists);
        dists.sort(function (p1, p2) {
            return p1.value - p2.value;
        });

        var topTenObj = dists.slice(0, 10).reduce(function (obj, prop) {
            obj[prop.key] = prop.value;
            return obj;
        }, {});
        var ids_to_label = Object.keys(topTenObj);
        Scatter.graphData().nodes.forEach(function (n) {
            var coord = {
                x: n.fx,
                y: n.fy,
                z: n.fz
            };
            var coordv = new Vector3(n.fx, n.fy, n.fz);

            if (frustum.containsPoint(coord)) {
                var dist = distanceVector(coord, camera.position); // convert vertical fov to radians  

                var height = 2 * Math.tan(vFOV / 2) * dist;
                var aspect = window.width / window.height;
                var width = height * aspect;
                var fraction = n.size * 15 / height;
                if (!fixedLabels) {

                    if (fraction > 0.40 && spriteysCanvas[n.id] === undefined && n.labeled !== 1) {
                        if (n.labeled === 2 && ids_to_label.indexOf(n.id) !== -1) {
                            spriteysCanvas[n.id].material.visible = true;
                        } else {
                            if (!htmlTextBool && ids_to_label.indexOf(n.id) !== -1) {
                                addLabel(n);
                            }
                            n.labeled = 2;
                        }
                    } else if (fraction > 0.4 && spriteysCanvas[n.id] !== undefined && n.labeled !== 1) {
                        if (ids_to_label.indexOf(n.id) !== -1) {
                            spriteysCanvas[n.id].material.visible = true;
                            n.labeled = 2;
                        }
                    } else if (fraction < 0.38 && spriteysCanvas[n.id] !== undefined && n.labeled !== 1) {
                        spriteysCanvas[n.id].material.visible = false;
                        n.labeled = -1;
                    }
                }

            }
        });
    }

    setTimeout(function () {
        updateLabel();
    }, 1);
}


function updateGeometries() {
    Scatter.nodeRelSize(4); // trigger update of 3d objects in scene
}

function strength(link) {
    return 1 / Math.min(count(link.source), count(link.target));
}

function count(node) {
    var links = Scatter.graphData().links;
    var c = 0;
    links.forEach(function (l) {
        if (l.source.id === node.id || l.target.id === node.id) {
            c++;
        }
    });
    return (c);
}

function openModuleColorPicker(ev, name) {
    current_module = ev.id;
    document.getElementById('selectId').innerHTML = name;
    if (ev.id.includes("meta")) {
        var inx = parseInt(ev.id.replace("meta_", ""));
    }
    current_point_color = $("#" + current_module).css("backgroundColor");
    var a = current_point_color.split("(")[1].split(")")[0];
    a = a.split(",");
    current_point_color = "#" + convertToHex(a);
    //if (current_point_color !== "#ffffff") {
    $("#customModule").spectrum("set", current_point_color);
    //}
}

function openModuleColorPickerAll(row, index, typeString) {
    document.getElementById('selectId').innerHTML = current_selected_row.name;
    current_module = typeString + "" + index;
    var inx2 = selectedMeta.indexOf(row.name);
    if (inx2 > -1) {
        selectedMeta.splice(inx2, 1);
    }
    selectedMeta.push(row.name);

    if (typeString.includes("cluster")) {
        current_point_color = highlightColor;
    } else {

        current_point_color = $("#" + current_module).css("backgroundColor");
        var a = current_point_color.split("(")[1].split(")")[0];
        a = a.split(",");
        current_point_color = "#" + convertToHex(a);
    }

    if (current_point_color !== "#ffffff") {
        $("#customModule").spectrum("set", current_point_color);
        current_encasing_color = current_point_color;
        $("#customEncasing").spectrum("set", current_point_color);
    }
}

function collisionRadius(node) {
    return (node.tsize);
}

const asc = arr => arr.sort((a, b) => a - b);

const sum = arr => arr.reduce((a, b) => a + b, 0);

const mean = arr => sum(arr) / arr.length;

// sample standard deviation
const std = (arr) => {
    const mu = mean(arr);
    const diffArr = arr.map(a => (a - mu) ** 2);
    return Math.sqrt(sum(diffArr) / (arr.length - 1));
};

const quantile = (arr, q) => {
    const sorted = asc(arr);
    const pos = ((sorted.length) - 1) * q;
    const base = Math.floor(pos);
    const rest = pos - base;
    if ((sorted[base + 1] !== undefined)) {
        return sorted[base] + rest * (sorted[base + 1] - sorted[base]);
    } else {
        return sorted[base];
    }
};

const q25 = arr => quantile(arr, .25);

const q50 = arr => quantile(arr, .50);

const q75 = arr => quantile(arr, .75);

const median = arr => q50(arr);



function getAllIndexes(arr, val) {
    var indexes = [],
            i = -1;
    while ((i = arr.indexOf(val, i + 1)) !== -1) {
        indexes.push(i);
    }
    return indexes;
}

function v(x, y, z) {
    return new Vector3(x, y, z);
}
var ellipsoidgeometry, ellipsoidmesh;

function initCube(scene) {
    //Postprocess();
    generateAxes(scene, 1100);
    initPlane(scene);

    if (gData.links.length === 0) {
        Scatter.graphData().nodes.forEach(function (n) {

        });
    } else {

        Scatter.graphData().nodes.forEach(function (n) {
            n.tcolor = n.color;
            colorNodeObj(n, n.color);
        });


    }

    var light = new SpotLight(0xffffff);
    light.intensity = 0.5;
    light.position.set(500, 2500, 0);
    light.lookAt(0, 0, 0);
    light.castShadow = true;
    light.userData.shadow = 1;

    light.shadow.mapSize.width = 2024;
    light.shadow.mapSize.height = 2024;

    light.shadow.camera.near = 10;
    light.shadow.camera.far = 5000;
    light.shadow.camera.fov = 30; // default

    Scatter.scene().traverse(function (child) {
        if (child instanceof DirectionalLight) {
            child.visible = false;
        }
        if (child instanceof AmbientLight) {
            child.intensity = 0.5;
        }
    });
    scene.add(light);

    var light = new SpotLight(0xd3d3d3);
    light.intensity = 0.5;
    light.position.set(-500, -2500, 0);
    light.lookAt(0, 0, 0);
    light.castShadow = true;
    light.userData.shadow = 1;

    light.shadow.mapSize.width = 2024;
    light.shadow.mapSize.height = 2024;

    light.shadow.camera.near = 10;
    light.shadow.camera.far = 5000;
    light.shadow.camera.fov = 30; // default

    Scatter.scene().traverse(function (child) {
        if (child instanceof DirectionalLight) {
            child.visible = false;
        }
        if (child instanceof AmbientLight) {
            child.intensity = 0.5;
        }
    });
    // scene.add(light);

    scene.add(new AmbientLight(0xFFFFFF));
    //var helper = new CameraHelper(light.shadow.camera)
    //Scatter.scene().add(helper);
}
//type: axis, tick, biplot
function createText2D(text, idModifier, sceneId, type) {
    //idmodifier in case there is more than one instance of the same text in the scene
    var myText = new SpriteText(text);
    if (myTexts[text + idModifier + sceneId] !== undefined) {
        myTexts[text + idModifier + sceneId].visible = false;
    }
    if (sceneId === scene2.uuid) {
        labelColor2 = getContrastColor(bgColor2.replace("#", ""));
        myText.color = labelColor2;
    } else {
        labelColor = getContrastColor(bgColor.replace("#", ""));
        myText.color = labelColor;
    }

    myText.visible = true;
    //myText.position.y = 15;
    myText.textHeight = 12;
    myText.fontSize = 100;
    myText.depthWrite = false;
    myText.userData = {};
    myText.userData.id = text;
    myText.userData.type = type;

    myTexts[text + idModifier + sceneId] = myText;
    return (myText);
}

function clearAllAxisTexts() {
    for (let key in myTexts) {
        if (myTexts.hasOwnProperty(key)) {
            let myText = myTexts[key];
            if (myText && myText.visible && myText.userData.type === 'axis') {
                myText.visible = false; // Hide the text
                // Optionally, you can remove it from the scene if necessary
                if (key.includes(scene2.uuid)) {
                    scene2.remove(myText);
                } else {
                    Scatter.scene().remove(myText);
                }
                delete myTexts[key]; // Remove the reference from myTexts
            }
        }
    }
}

function ellipsoid() {
    var latitudeBands = 30,
            longitudeBands = 20,
            a = 6,
            b = 7,
            c = 20,
            size = 40;
    for (var latNumber = 0; latNumber <= latitudeBands; latNumber++) {
        var theta = (latNumber * Math.PI * 2 / latitudeBands);
        var sinTheta = Math.sin(theta);
        var cosTheta = Math.cos(theta);

        for (var longNumber = 0; longNumber <= longitudeBands; longNumber++) {
            var phi = (longNumber * 2 * Math.PI / longitudeBands);
            var sinPhi = Math.sin(phi);
            var cosPhi = Math.cos(phi);


            var x = a * cosPhi * cosTheta;
            var y = b * cosTheta * sinPhi;
            var z = c * sinTheta;
            ellipsoidgeometry.vertices.push(new Vector3(x * size, y * size, z * size));

        }


    }
    for (var latNumber = 0; latNumber < latitudeBands; latNumber++) {
        for (var longNumber = 0; longNumber < longitudeBands; longNumber++) {
            var first = (latNumber * (longitudeBands + 1)) + longNumber;
            var second = first + longitudeBands + 1;
            ellipsoidgeometry.faces.push(new Face3(first, second, first + 1));
            ellipsoidgeometry.faces.push(new Face3(second, second + 1, first + 1));

        }
    }
}


function computeEllipsoid(meshObj, scene, ids, color, type, group = "NA", alphaVal) {
    if (group !== "NA") {
        current_module = group;
    }
    if (current_module !== "NA" && current_module !== null) {
        deleteMetaSphere(current_module, scene);
        contours_info_obj[current_module] = {
            ids: ids,
            color: color,
            type: type
        };
    }
    shape_type = type;

    var col;
    var mesh;
    console.log(meshObj)
    for (var i = 0; i < meshObj.length; i++) {
        var ob_orig = meshObj[i].vb;
        var ob = [];
        var points = [];
        var xArr = [];
        var yArr = [];
        var zArr = [];

        //address odd issue of array formatting difference after reloading from report
        if (!Array.isArray(ob_orig) || !Array.isArray(ob_orig[0]) || !Array.isArray(ob_orig[1]) || !Array.isArray(ob_orig[2])) {
            ob = Array.from({length: 4}, () => []);
            for (let i = 0; i < ob_orig.length; i++) {
                const targetSubArray = i % 4;
                ob[targetSubArray].push(ob_orig[i]);
            }
        } else {
            ob = ob_orig;
        }
        for (var j = 0; j < ob[0].length; j++) {
            points.push(v(ob[0][j] * 1000, ob[1][j] * 1000, ob[2][j] * 1000));
        }
        var geom = new ConvexGeometry(points);
        var opp = 0.7;
        opp = opp + 0.3;
        var tr = true;
        if (opp > 0.7) {
            opp = 0.7;
            col = 0x431c53;
        } else {
            if (opp > 0.6) {
                opp = 1;
                col = 0xDA70D6;
            } else {
                opp = 0.25;
                col = 0xDA70D6;
            }
            tr = true;
        }
        var material = new MeshPhongMaterial({
            color: color,
            transparent: true,
            alphaTest: 0.1,
            opacity: alphaVal,
            side: BackSide
        });
        mesh = new Mesh(geom, material);
        var nds;
        if (current_main_plot === "score") {
            var nds = Scatter.graphData().nodes;
        } else {
            var nds = gData.navigation;
        }
        nds.forEach(function (n) {
            if (ids.indexOf(n.id) !== -1) {
                xArr.push(n.fx);
                yArr.push(n.fy);
                zArr.push(n.fz);
            }
        });
        var meanX = meanF(xArr);
        var meanY = meanF(yArr);
        var meanZ = meanF(zArr);
        var meanXYZ = v(meanX, meanY, meanZ);

        //geom.mergeVertices();
        //geom.computeVertexNormals()
        var clusterGroup = new Object3D();
        mesh.userData.ids = ids;
        mesh.userData.centroid = meanXYZ;
        mesh.userData.shadow = 1;
        mesh.castShadow = false;
        mesh.receiveShadow = false;
        //mesh.scale.set(1000, 1000, 1000)
        mesh.focused = false;
        var colorValue = parseInt(color.replace("#", "0x"), 16);
        var colored = new Color(colorValue);
        mesh.userData.tcolor = colored;
        mesh.userData.tcolorhex = color;
        mesh.userData.opacity = opp;
        clusterGroup.userData = ids;
        clusterGroup.centroid = meanXYZ;
        //mesh.scale.set(1000, 1000, 1000)
        if (type === "ellipse") {
            mesh.position.set(meanX, meanY, meanZ);
        } else {
            mesh.position.set(0, 0, 0);
        }
        clusterGroup.focused = false;
        clusterGroup.add(mesh);
        scene.add(clusterGroup);
        boundingClusters[current_module] = mesh;
        //boundingClustersGroup[current_module] = clusterGroup;
    }
    var sceneCenter = v(0, 0, 0);
    for (var propertyNm in boundingClustersGroup) {
        var obj = boundingClustersGroup[propertyNm];
        obj.clusterNum = propertyNm;
        var clusterCenter = obj.centroid;
        var dir = new Vector3(); // create once an reuse it
        dir.subVectors(clusterCenter, sceneCenter).normalize();

        // scalar to simulate speed
        var vector = dir.multiplyScalar(1000, 1000, 1000);
        obj.explosionDirection = vector;
        obj.targetPosition = new Vector3();
        obj.targetPosition.x += obj.explosionDirection.x;
        obj.targetPosition.y += obj.explosionDirection.y;
        obj.targetPosition.z += obj.explosionDirection.z;
        obj.targetPositionCopy = JSON.parse(JSON.stringify(obj.targetPosition));

        Scatter.graphData().nodes.forEach(function (n) {
            if (obj.userData.indexOf(n.id) !== -1) {
                // n.explosionDirection = obj.explosionDirection
                //obj.add(n.__threeObj)
            }
        });
}
}

function meanF(arr) {
    //Find the sum
    var sum = 0;
    for (var i in arr) {
        sum += arr[i];
    }
    //Get the length of the array
    var numbersCnt = arr.length;
    //Return the average / mean.

    return (sum / numbersCnt);
}

function computeContours(meshObj, scene, ids, color, group, alphaVal) {

    if (group !== "NA") {
        current_module = group;
    }
    if (current_module !== "NA" && current_module !== null) {
        deleteMetaSphere(current_module, scene);
        contours_info_obj[current_module] = {
            ids: ids,
            color: color,
            type: "contour"
        };
    }



    if (meshObj === "NA") {
        return;
    }
    var colObj = {};
    var newObj = {};
    for (var propNm in meshObj) {
        var ob = meshObj[propNm];
        if (ob.type === "triangles") {
            var col = "rgb(" + Math.round(ob.colors[0]["r"] * 255) + ", " + Math.round(ob.colors[0]["g"] * 255) + ", " + Math.round(ob.colors[0]["b"] * 255) + ")";
            var a = col.split("(")[1].split(")")[0];
            a = a.split(",");
            var colHex = "#" + convertToHex(a);
            var clusterInx = Set2.indexOf(colHex);
            if (newObj[clusterInx] === undefined) {
                newObj[clusterInx] = [];
            }
            newObj[clusterInx].push(...ob.vertices);
            colObj[clusterInx] = colHex;
        }
    }

    var xArr = [];
    var yArr = [];
    var zArr = [];
    for (var propNm in newObj) {
        if (Object.prototype.hasOwnProperty.call(newObj, propNm)) {
            var ob = newObj[propNm];
            var geomm = new BufferGeometry();
            //var vertices = [];
            //var faces = [];

            var xyzArr = [];
            var normals = [];
            for (var i = 0; i < ob.length; i++) {
                const vv = ob[i];
                const xpos = vv.x * 1000;
                const ypos = vv.y * 1000;
                const zpos = vv.z * 1000;

                xyzArr.push(xpos);
                xyzArr.push(ypos);
                xyzArr.push(zpos);

                normals.push(i);
                normals.push(i + 1);
                normals.push(i + 2);
            }


            geomm.setAttribute('position', new Float32BufferAttribute(xyzArr, 3));
            geomm.setAttribute('normal', new Float32BufferAttribute(normals, 3));


            var meanX = meanF(xArr);
            var meanY = meanF(yArr);
            var meanZ = meanF(zArr);
            var mean = v(meanX, meanY, meanZ);

            var tr = true;

            var material = new MeshPhongMaterial({
                transparent: tr,
                color: color,
                alphaTest: 0.1,
                opacity: alphaVal,
                depthTest: true,
                depthWrite: true
            });
            var mesh = new Mesh(geomm, material);
            geomm = mergeVertices(geomm);
            //geomm.computeVertexNormals();
            //mesh.scale.set(1000, 1000, 1000)
            mesh.position.set(0, 0, 0);
            var nds;
            if (scene === Scatter.scene()) {
                nds = Scatter.graphData().nodes;
            } else {
                nds = nav_nodes_arr;
            }

            var nodes_vec = [];
            nds.forEach(function (n) {
                if (n.cluster === parseInt(propNm) + 1) {
                    nodes_vec.push(n.id);
                }
            });
            scene.add(mesh);
            //}
            var clusterGroup = new Object3D();
            mesh.userData.ids = ids;
            mesh.userData.centroid = mean;
            mesh.userData.shadow = 1;
            mesh.castShadow = false;
            mesh.receiveShadow = false;
            //mesh.scale.set(1000, 1000, 1000)
            mesh.focused = false;
            var colorValue = parseInt(color.replace("#", "0x"), 16);
            var colored = new Color(colorValue);
            mesh.userData.tcolor = colored;
            mesh.userData.tcolorhex = color;
            mesh.userData.opacity = 0.3;
            clusterGroup.userData = ids;
            clusterGroup.centroid = mean;
            //mesh.scale.set(1000, 1000, 1000)
            clusterGroup.focused = false;
            mesh.position.set(0, 0, 0);
            clusterGroup.add(mesh);
            scene.add(clusterGroup);
            boundingClusters[current_module] = mesh;
            boundingClustersGroup[current_module] = clusterGroup;
        }
    }

    Scatter.graphData().nodes.forEach(function (n) {
        xArr.push(n.fx);
        yArr.push(n.fy);
        zArr.push(n.fz);
    });
    var meanX = meanF(xArr);
    var meanY = meanF(yArr);
    var meanZ = meanF(zArr);
    var sceneCenter = v(meanX, meanY, meanZ);

    for (var propertyNm in boundingClustersGroup) {
        var obj = boundingClustersGroup[propertyNm];
        obj.clusterNum = propertyNm;
        var clusterCenter = obj.centroid;
        var dir = new Vector3(); // create once an reuse it
        dir.subVectors(clusterCenter, sceneCenter).normalize();

        // scalar to simulate speed
        var vector = dir.multiplyScalar(1000, 1000, 1000);
        obj.explosionDirection = vector;
        obj.targetPosition = new Vector3();
        obj.targetPosition.x += obj.explosionDirection.x;
        obj.targetPosition.y += obj.explosionDirection.y;
        obj.targetPosition.z += obj.explosionDirection.z;
        obj.targetPositionCopy = JSON.parse(JSON.stringify(obj.targetPosition));
        Scatter.graphData().nodes.forEach(function (n) {
            if (obj.userData.indexOf(n.id) !== -1) {
                //n.explosionDirection = obj.explosionDirection
                //obj.add(n.__threeObj)
            }
        });
    }
}

function selectCluster(e) {
    if (!e.focused) {
        e.focused = true;
        var i = 0;
        var n;
        //var narr = e.userData.ids

        var arr = [];
        var count = 0;
        for (var propertyNm in boundingClusters) {
            var obj = boundingClusters[propertyNm];
            if (obj.focused && count === 0) {
                arr = obj.userData.ids;
                count++;
            } else if (obj.focused) {
                arr = arr.concat(obj.userData.ids);
            }
        }

        Scatter.graphData().nodes.forEach(function (nd) {
            if (arr.indexOf(nd.id) === -1) {
                nd.highlight = 0;
                colorNodeObj(nd, "#d3d3d3");
                opaNodeObj(nd, 0.2);
                deleteSprite(nd);
                deleteOutline(nd);
                if (nd.outline === 1) {
                    nd.outline = -1;
                }
                //nd.module = 2
            } else {
                nd.highlight = 1;
                colorNodeObj(nd, nd.colorb);
                opaNodeObj(nd, 1);
                if (nd.labeled === 0) {
                    addLabel(nd);
                }
                if (nd.outline === -1) {
                    outlineNode(nd, highlightColor);
                }
            }
        });

        //Scatter.cooldownTicks(100)

        for (var propertyNm in boundingClusters) {
            if (!boundingClusters[propertyNm].focused) {
                var colorValue = parseInt("#d6d6d6".replace("#", "0x"), 16);
                var colored = new Color(colorValue);
                boundingClusters[propertyNm].material.color = colored;
                boundingClusters[propertyNm].material.opacity = boundingClusters[propertyNm].material.opacity - 0.3;
            } else {
                boundingClusters[propertyNm].visible = true;
                boundingClusters[propertyNm].material.opacity = boundingClusters[propertyNm].userData.opacity;
                boundingClusters[propertyNm].material.color = boundingClusters[propertyNm].userData.tcolor;
            }
        }

        Scatter.controls().target.set(e.userData.centroid.x, e.userData.centroid.y, e.userData.centroid.z);
        currentOrigin = v(e.userData.centroid.x, e.userData.centroid.y, e.userData.centroid.z);
        displayNodeInfoModule(propertyNm + "");
    } else {
        e.focused = false;
        var arr = [];
        for (var propertyNm in boundingClusters) {
            var obj = boundingClusters[propertyNm];
            if (obj.focused) {
                arr = arr.concat(obj.userData.ids);
            }
        }

        Scatter.graphData().nodes.forEach(function (nd) {
            if (arr.length > 0) {
                if (arr.indexOf(nd.id) !== -1) {
                    nd.highlight = 1;
                    colorNodeObj(nd, nd.tcolor);
                    opaNodeObj(nd, 1);
                    if (nd.labeled === 0) {
                        addLabel(nd);
                    }
                    if (nd.outline === -1) {
                        outlineNode(nd, highlightColor);
                    }
                } else {
                    nd.highlight = 0;
                    colorNodeObj(nd, "#d3d3d3");
                    opaNodeObj(nd, 0.2);
                    deleteSprite(nd);
                    deleteOutline(nd);
                    if (nd.outline === 1) {
                        nd.outline = -1;
                    }
                }
            } else {
                nd.highlight = 0;
                colorNodeObj(nd, nd.tcolor);
                opaNodeObj(nd, 1);
                if (nd.labeled === 0) {
                    addLabel(nd);
                }
                if (nd.outline === -1) {
                    outlineNode(nd, highlightColor);
                }
            }
        });
        //Scatter.cooldownTicks(100)
        for (var propertyNm in boundingClusters) {
            if (arr.length > 0) {
                if (boundingClusters[propertyNm].focused) {
                    boundingClusters[propertyNm].visible = true;
                    boundingClusters[propertyNm].material.opacity = boundingClusters[propertyNm].userData.opacity;
                    boundingClusters[propertyNm].material.color = boundingClusters[propertyNm].userData.tcolor;
                } else {
                    var colorValue = parseInt("#d6d6d6".replace("#", "0x"), 16);
                    var colored = new Color(colorValue);
                    boundingClusters[propertyNm].material.color = colored;
                    boundingClusters[propertyNm].material.opacity = boundingClusters[propertyNm].material.opacity - 0.3;
                }
            } else {
                boundingClusters[propertyNm].visible = true;
                boundingClusters[propertyNm].material.opacity = boundingClusters[propertyNm].userData.opacity;
                boundingClusters[propertyNm].material.color = boundingClusters[propertyNm].userData.tcolor;
            }
        }
        Scatter.controls().target.set(0, 0, 0);
        currentOrigin = v(0, 0, 0);
        displayNodeInfoModule(propertyNm + "");
    }
}

function updateMetaInfo(index, row) {
    current_selected_row = row;
    openModuleColorPickerAll(row, index, "meta_");
    document.getElementById('selectId').innerHTML = current_selected_row.name;
}

function updateCustomInfo(index, row) {
    current_selected_row = row;
    openModuleColorPickerAll(row, index, "cluster_");
    document.getElementById('selectId').innerHTML = current_selected_row.name;
}

function loadMeta() {
    if (gData.metaCol.type === "gradient") {
        legendUtils.generateGradientLegend(gData.metaCol);
    } else {
        let mdl_rows = [];
        for (var i = 0; i < gData.metaCol.label.length; i++) {
            mdl_rows.push({
                "label": gData.metaCol.label[i],
                "color": gData.metaCol.color[i]
            });
        }
        legendUtils.generateLegend(mdl_rows);
    }

    if (gData.metaShape !== undefined) {
        let mdl_rows2 = [];

        for (var i = 0; i < gData.metaShape.shape.length; i++) {
            mdl_rows2.push(
                    {
                        "label": gData.metaShape.label[i],
                        "shape": gData.metaShape.shape[i]
                    }
            )
        }
        legendUtils.generateLegendShape(mdl_rows2);
    }


}



function highlightSelectedMeta(name, color, haloBool, encasingType) {
    var sizeval = parseInt($("#nsize").val());
    if (current_main_plot === "score") {
        var ids = [];
        Scatter.graphData().nodes.forEach(function (n) {

            if (n.metatype === name) {
                ids.push(n.id);
                n.highlight = 1;
                n.colorb = color;
                n.tcolor = color;
                colorNodeObj(n, color);
                sizeNodeObj(n, sizeval / 2);
                if (haloBool) {
                    outlineNode(n, current_halo_color);
                } else {
                    deleteOutline(n);
                }
            }
        });
        Scatter.graphData().links.forEach(function (l) {
            colorLineObj2(l, l.color);
        });
        if (encasingType !== "na") {
            computeEncasing(ids, encasingType, current_encasing_color, current_module, 0.95, 0.5, name);
        } else {
            deleteMetaSphere(current_module, Scatter.scene());
        }
    } else {
        var sc = scene2;
        sc.traverse(function (child) {
            if (child instanceof Mesh) {
                if (child.userData.nodeData !== undefined) {

                    var n = child.userData.nodeData;
                    if (n.metatype === name) {
                        n.highlight = 1;
                        n.tcolor = color;
                        colorNodeObjScene2(child, color);
                        if (haloBool) {
                            //outlineNode(child, color)
                        }
                    }
                }
            }
        });
    }
}

function unhighlightSelectedMeta(name) {
    if (current_main_plot === "score") {
        Scatter.graphData().nodes.forEach(function (n) {
            if (n.metatype === name) {
                n.highlight = 0;
                if (n.tcolor === undefined) {
                    colorNodeObj(n, n.colorb);
                } else {
                    ;
                    colorNodeObj(n, n.tcolor);
                }
                deleteOutline(n);
            }
        });
        Scatter.graphData().links.forEach(function (l) {
            colorLineObj2(l, l.color);
        });
    } else {
        var sc = scene2;
        sc.traverse(function (child) {
            if (child instanceof Mesh) {
                if (child.userData.nodeData !== undefined) {

                    var n = child.userData.nodeData;
                    if (n.metatype === name) {
                        n.highlight = 1;
                        colorNodeObjScene2(child, n.tcolor);
                    }
                }
            }
        });
    }
    var stats = $("#stats");
    stats.empty();
}

function getBoxPlotOld(id) {
    var selected_grp;
    var selected_factor;
    var opt = $('#compopt').val();
    if (opt === "cluster") {
        selected_factor = "cluster";
        curr_meta = "cluster";
        var rows = $('#mdg').datagrid('getSelections');
        var selectedMeta = [];
        for (var i = 0; i < rows.length; i++) {
            selectedMeta.push(rows[i].name.split("Cluster ")[1]);
        }
    } else {
        curr_meta = $("#metaopt").val();
        var rows = $('#metadg').datagrid('getSelections');
        var selectedMeta = [];
        for (var i = 0; i < rows.length; i++) {
            selectedMeta.push(rows[i].name);
        }
    }
    selected_factor = curr_meta;
    for (var i = 0; i < selectedMeta.length; i++) {
        if (i === 0) {
            selected_grp = selectedMeta[i];
        } else {
            selected_grp = selected_grp + "; " + selectedMeta[i];
        }
    }
    $.ajax({
        beforeSend: function () {
            $.messager.progress({
                text: 'Processing .....'
            });
        },
        dataType: "html",
        type: "POST",
        url: '/OmicsAnalyst/faces/AjaxCall',
        data: {
            function: 'plotSelectedGene',
            id: id,
            meta: selected_factor,
            selected: selected_grp
        },
        async: false,
        success: function (result) {
            $.messager.progress('close');
            $("#stats").empty();
            var boxplot = $("#dragDiv");
            boxplot.css('display', 'block');
            document.getElementById('image').src = usr_dir + "/" + result;
        }
    });
}

function updateSampleBasedOnLoading(id) {

    var omicstype = "NA";
    if (["mcia", "procrustes", "mbpca", "diablo", "spls", "rcca"].indexOf(gData.reductionOpt) !== -1) {
        omicstype = comparison_omicstype;
        if (omicstype === "omics") {
            $.messager.alert('', "Please select another meta data other than omics type!", 'error');
            return;
        }
    }
    $.ajax({
        beforeSend: function () {

        },
        dataType: "html",
        type: "POST",
        url: '/OmicsAnalyst/faces/AjaxCall',
        data: {
            function: 'updateSampleBasedOnLoading',
            id: id,
            omicstype: omicstype
        },
        async: false,
        success: function (result) {
            $.getJSON(usr_dir + "/" + result, function (jsonDat) {
                if (current_main_plot === "score") {

                    if (omicstype !== "NA") {
                        var i = 0;
                        if (Scatter.graphData().nodes.length === jsonDat.length) {
                            Scatter.graphData().nodes.forEach(function (n) {
                                if (i < jsonDat.length) {
                                    colorNodeObj(n, jsonDat[i]);
                                    i++;
                                }
                            });
                        } else {
                            Scatter.graphData().nodes.forEach(function (n) {
                                if (n.type === omicstype) {
                                    colorNodeObj(n, jsonDat[i]);
                                    i++;
                                } else {
                                    colorNodeObj(n, "#d3d3d3");
                                    opaNodeObj(n, 0.3);
                                }
                            });
                        }
                    } else {
                        var i = 0;
                        Scatter.graphData().nodes.forEach(function (n) {
                            if (i < jsonDat.length) {
                                colorNodeObj(n, jsonDat[i]);
                                i++;
                            }
                        });
                    }
                    Scatter.graphData().links.forEach(function (l) {
                        l.highlight = 0;
                        l.width = 0;
                        colorLineObj2(l, edgeColor);
                        sizeLineObj(l, 0);
                    });
                } else {
                    var i = 0;
                    scene2.traverse(function (child) {
                        if (child instanceof Mesh) {
                            if (child.userData.nodeData !== undefined) {
                                colorNodeObjScene2(child, jsonDat[i]);
                                i++;
                            }
                        }
                    });
                }
            });
        }
    });
}

function computeEncasing(ids, type, color, group = "NA", level = 0.5, alphaVal = 0.5, reloadFlag = false, grpName = "NA") {
    if (ids.length < 10 && type === "contour") {

        $.messager.alert('Error', 'At least 10 data points are required for computing density contour!', 'error');
        return;
    }

    if (ids.length < 4 && type === "ellipse") {
        $.messager.alert('Warning', group + ': At least 4 data points are required for computing ellipse!', 'warning');
        return;
    }

    var ids_string = ids[0];
    for (var i = 1; i < ids.length; i++) {
        ids_string = ids_string + "; " + ids[i];
    }
    var omicstype = "NA";

    if (!reloadFlag) {
        $.ajax({
            beforeSend: function () {
                /*$.messager.progress({
                 text: 'Processing .....'
                 });*/
            },
            dataType: "html",
            type: "POST",
            url: '/MetaboAnalyst/faces/AjaxCall',
            data: {
                function: 'computeEncasing',
                type: type,
                names: ids_string,
                level: level,
                omics: omicstype
            },
            async: false,
            success: function (result) {
                $.getJSON(usr_dir + "/" + result, function (jsonDat) {
                    savedState.encasingFileNames[grpName] = result;
                    var sc;
                    sc = Scatter.scene();
                    if (type === "ellipse" || type === "alpha") {
                        gData.ellipse = jsonDat;
                        computeEllipsoid(jsonDat, sc, ids, color, type, group, alphaVal);
                    } else {
                        gData.objects = jsonDat;
                        computeContours(jsonDat, sc, ids, color, group, alphaVal);
                    }

                });
                $("#loader").hide();
            },
            error: function () {
                $.messager.alert('Error', 'Failed to process the request!', 'error');
                $.messager.progress('close');
            }
        });
    } else {
        //console.log("savedState.encasingFileNames===" + savedState.encasingFileNames)
        const fileName = savedState.encasingFileNames[grpName];
        $.getJSON(usr_dir + "/" + fileName, function (jsonDat) {
            var sc;
            sc = Scatter.scene();
            if (type === "ellipse" || type === "alpha") {
                gData.ellipse = jsonDat;
                computeEllipsoid(jsonDat, sc, ids, color, type, group, alphaVal);
            } else {
                gData.objects = jsonDat;
                computeContours(jsonDat, sc, ids, color, group, alphaVal);
            }

        });
}
}


function doClustering(callBack) {
    var opt = $('#clusteralg').val();
    var analopt = "all";
    var meta = $('#metaopt').val();
    var clusterNb = $('#clusterNb').val();
    var algOpts = ["peak", "meanshift", "kmeans"];
    if (algOpts.indexOf(meta) !== -1) {
        opt = meta;
        meta = curr_meta;
    } else {
        curr_meta = meta;
        loadMeta();
        return;
    }

    $.ajax({
        beforeSend: function () {
            $.messager.progress({
                text: 'Processing .....'
            });
        },
        dataType: "html",
        type: "POST",
        url: '/OmicsAnalyst/faces/AjaxCall',
        data: "function=performClusteringMeta" + "&opt=" + opt + "&meta=" + meta + "&anal.opt=" + analopt + "&cluster.nb=" + clusterNb,
        async: true,
        cache: false,
        success: function (result) {
            if (result === "NA") {
                $.messager.alert('Error', 'Errors occured in the function: performClusteringScatter.', 'error');
                $.messager.progress('close');
                return callBack(result);
            } else {
                $.messager.progress('close');
                return callBack(result);
            }
        },
        error: function () {
            $.messager.alert('Error', 'Failed to process the request!', 'error');
            $.messager.progress('close');
        }
    });
}

function performClusteringScatter() {
    var alg = $('#clusteralg').val();
    var selmeta = $('#metaopt').val();
    var nodes = [];

    doClustering(function (res) {
        $.getJSON(usr_dir + "/" + res, function (jsonDat) {
            var selmeta = curr_meta;
            var analopt = $('#analopt').val();
            var minclust = parseInt($('#minclust').val());
            var clusters = {};
            var nodes = [];

            if (current_main_plot === "score") {
                nodes = Scatter.graphData().nodes;
            } else {
                nodes = [];
                scene2.traverse(function (child) {
                    if (child instanceof Mesh) {
                        if (child.userData.nodeData !== undefined) {
                            nodes.push(child.userData.nodeData);
                        }
                    }
                });
            }

            for (var i = 0; i < jsonDat.cluster.length; i++) {
                for (var j = 0; j < jsonDat.cluster[i].inxs.length; j++) {
                    var n = nodes[jsonDat.cluster[i].inxs[j] - 1];
                    n.cluster = jsonDat.cluster[i].cluster[j];
                    //if (["kmeans", "meanshift", "peak"].indexOf(selmeta) === -1) {
                    n.metatype = "Cluster " + jsonDat.cluster[i].cluster[j];
                    //}
                    // 
                }
            }

            for (var i = 0; i < nodes.length; i++) {
                var n = nodes[i];
                if (clusters[n.cluster] === undefined) {
                    clusters[n.cluster] = [];
                }
                clusters[n.cluster].push(n.id);

            }

            var propNms = Object.keys(clusters);
            /*
             for (var i = 0; i < nodes.length; i++) {
             var n = nodes[i]
             n.metatype = jsonDat.meta[selmeta][i]
             var inx = jsonDat.metaNm.indexOf(n.metatype)
             }*/
            getColorArr(clusters);
            gData.meta = jsonDat.meta;
            gData.ellipse = jsonDat.ellipse;
            //gData.objects = jsonDat.objects
            if (shape_type === "ellipse") {
                //computeEllipsoid(Scatter.scene());
            } //else {
            //    computeContours(Scatter.scene());
            //}
            $.messager.progress('close');
        });
    });

}

function updateClusterInfo(index, row) {
    current_selected_row = row.name;
    openModuleColorPickerAll(row, index, "meta_");
    var e = cluster_arr[index];
    if (e !== undefined) {
        cluster_arr[index].focused = false;
        selectClusterNew(e);
    }

    updateCellColor(highlightColor, "meta_" + index);
}

function displayPatternData(row, type) {
    $("#dragDiv").css('display', 'none');
    //$('#p').panel('open')
    var stats = $("#stats");
    var title = row.name;
    var nodes = [];

    if (type === "metatype" || type === "meta") {
        Scatter.graphData().nodes.forEach(function (n) {
            if (n[type] === row.name) {
                nodes.push(n);
            }
        });
    } else {
        Scatter.graphData().nodes.forEach(function (n) {
            if (n[type] === row.index + 1) {
                nodes.push(n);
            }
        });
    }

    stats.empty();
    if (title !== "") {
        stats.append('<lh><b>' + title + '</b></lh>');
    }
    for (var i = 0; i < nodes.length; i++) {
        stats.append('<li class="stats">' + nodes[i].label + '</li>');
    }
}

function disposeMesh(mesh, scene) {
    if (mesh.material !== undefined) {
        mesh.visible = false;
        mesh.scale.set(1, 1, 1);
        mesh.material.dispose();
        mesh.geometry.dispose();
        mesh.geometry = undefined;
        mesh.material = undefined;
        scene.remove(mesh);
    }
}


function deleteMetaSphere(inx, scene) {
    if (boundingClusters[inx] !== undefined) {
        var mesh = boundingClusters[inx];
        mesh.visible = false;
        mesh.scale.set(1, 1, 1);
        scene.remove(mesh);
        mesh.material.dispose();
        mesh.geometry.dispose();
        mesh.geometry = undefined;
        mesh.material = undefined;
        delete boundingClustersGroup[inx];
        delete boundingClusters[inx];
        delete activeModules[inx];
    }
}

function generateAxisLabel(scene) {
    var length = 1100;
    var xString = "Comp.1";
    var yString = "Comp.2";
    var zString = "Comp.3";

    //labelArr = {};
    if (current_main_plot === "score") {
        if (scene === Scatter.scene()) {
            xString = gData.axis[0];
            yString = gData.axis[1];
            zString = gData.axis[2];

        } else {
            xString = gData.axisLoading[0];
            yString = gData.axisLoading[1];
            zString = gData.axisLoading[2];
        }
    } else {
        if (scene === scene2) {
            xString = gData.axis[0];
            yString = gData.axis[1];
            zString = gData.axis[2];

        } else {
            xString = gData.axisLoading[0];
            yString = gData.axisLoading[1];
            zString = gData.axisLoading[2];
        }
    }

    var titleX = createText2D(xString, "", scene.uuid, "axis");
    scene.add(titleX);
    labelArr["x"] = titleX;

    var titleX = createText2D(yString, "", scene.uuid, "axis");
    scene.add(titleX);
    labelArr["y"] = titleX;

    if (!twoDMode) {
        var titleZ = createText2D(zString, "", scene.uuid, "axis");
        scene.add(titleZ);
        labelArr["z"] = titleZ;
    } else {
        myTexts["Z" + "" + scene.uuid].visible = false;
        delete myTexts["Z" + "" + scene.uuid];
        scene.remove(myTexts["Z" + "" + scene.uuid]);
    }


    labelArr["x"].position.set(0, -length - 20, length + 450);
    labelArr["y"].position.set(-length - 20, 0, length + 300);
    labelArr["y"].material.rotation = Math.PI / 2;
    labelArr["z"].position.set(length + 450, -length - 20, 0);

}

function removeAxes(scene) {
    if (axesArr[scene.uuid] !== undefined) {
        for (var propertyNm in axesArr[scene.uuid]) {
            var pl = axesArr[scene.uuid][propertyNm];
            disposeMesh(pl.line, scene);
            disposeMesh(pl.cone, scene);
            pl = undefined;
            axesArr[scene.uuid][propertyNm] = undefined;
            scene.remove(axesArr[scene.uuid][propertyNm]);
            delete axesArr[scene.uuid][propertyNm];
        }
    }
}

function generateAxes(scene, length) {
    return;
    var type = $("#axisType").val();
    var textDisplay = $("#labelOpt").val();

    var dir = new Vector3(1, 0, 0);
    var origin = new Vector3(-length, 0, 0);
    var colorValue = parseInt(axisColorU.replace("#", "0x"), 16);
    var colored = new Color(colorValue);

    if (axesArr[scene.uuid] !== undefined) {
        for (var propertyNm in axesArr[scene.uuid]) {
            var pl = axesArr[scene.uuid][propertyNm];

            disposeMesh(pl.line, scene);
            disposeMesh(pl.cone, scene);
            pl = undefined;
            axesArr[scene.uuid][propertyNm] = undefined;
            scene.remove(axesArr[scene.uuid][propertyNm]);
            delete axesArr[scene.uuid][propertyNm];
        }
    }
    axesArr[scene.uuid] = {};
    var arrow = new ArrowHelper(dir, origin, 2 * length, colored, 0.1 * length, 0.4 * 0.1 * length);
    var arrow2 = new ArrowHelper(v(0, 1, 0), v(0, -length, 0), 2 * length, colored, 0.1 * length, 0.4 * 0.1 * length);
    var arrow3 = new ArrowHelper(v(0, 0, 1), v(0, 0, -length), 2 * length, colored, 0.1 * length, 0.4 * 0.1 * length);
    scene.add(arrow);
    scene.add(arrow2);

    axesArr[scene.uuid]["x"] = arrow;
    axesArr[scene.uuid]["y"] = arrow2;
    if (!twoDMode) {

        axesArr[scene.uuid]["z"] = arrow3;
        scene.add(arrow3);
        arrow3 = axesArr[scene.uuid]["z"];
    }

    arrow = axesArr[scene.uuid]["x"];
    arrow2 = axesArr[scene.uuid]["y"];

    if (type === "bottom") {
        if (!twoDMode) {
            arrow.position.set(-length, -length, -length);
            arrow2.position.set(-length, -length, -length);
            arrow3.position.set(-length, -length, -length);
        } else {
            arrow.position.set(-length, -length, 0);
            arrow2.position.set(-length, -length, 0);
            arrow3.position.set(-length, -length, 0);
        }
    } else {
        if (!twoDMode) {
            arrow.position.set(-length, 0, 0);
            arrow2.position.set(0, -length, 0);
            arrow3.position.set(0, 0, -length);
        } else {
            arrow.position.set(-length, 0, 0);
            arrow2.position.set(0, -length, 0);
        }

    }
    generateAxisLabel(scene);
    var sceneCenter = v(0, 0, 0);
    var type1 = $("#axisType").val();
    if (twoDMode) {
        sceneCenter = v(-length, -length, 0);
    } else if (type1 === "bottom") {
        sceneCenter = v(-length, -length, -length);
    }
    for (var propertyNm in boundingClustersGroup) {
        var obj = boundingClustersGroup[propertyNm];
        obj.clusterNum = propertyNm;
        var clusterCenter = obj.centroid;
        var dir = new Vector3(); // create once an reuse it
        dir.subVectors(clusterCenter, sceneCenter).normalize();

        // scalar to simulate speed
        var speed = 0.05;
        var vector = dir.multiplyScalar(1000, 1000, 1000);
        obj.explosionDirection = vector;
        obj.targetPosition = new Vector3();
        obj.targetPosition.x += obj.explosionDirection.x;
        obj.targetPosition.y += obj.explosionDirection.y;
        obj.targetPosition.z += obj.explosionDirection.z;
        obj.targetPositionCopy = JSON.parse(JSON.stringify(obj.targetPosition));

        Scatter.graphData().nodes.forEach(function (n) {
            if (obj.userData.indexOf(n.id) !== -1) {
                //n.explosionDirection = obj.explosionDirection
                //obj.add(n.__threeObj)
            }
        });
    }
}

function initWall(scene) {
    var colorValue = parseInt(wallCol.replace("#", "0x"), 16);
    var colored = new Color(colorValue);
    if (cubeU[scene.uuid] !== undefined) {
        disposeMesh(cubeU[scene.uuid], scene);
        cubeU[scene.uuid] = undefined;
    }
    var length = 1100;
    var geometry = new BoxGeometry(length * 2 + 50, length * 2 + 50, length * 2 + 50);
    var material = new MeshLambertMaterial({
        color: colored,
        side: BackSide
    });
    cubeU[scene.uuid] = new Mesh(geometry, material);
    cubeU[scene.uuid].receiveShadow = true;
    cubeU[scene.uuid].castShadow = false;
    cubeU[scene.uuid].userData = "wall";
    scene.add(cubeU[scene.uuid]);
}

function initGrids(scene, nodes, type, visible) {
    const length = 1000;
    const tickDivisions = 11;

    const minMax = minMaxCoords(nodes);
    //var min = [minMax.minX, minMax.minY, minMax.minZ];
    //var max = [minMax.maxX, minMax.maxX, minMax.maxX];

    var ticksX = [];
    var ticksY = [];
    var ticksZ = [];
    if (type === "main") {
        ticksX = gData.ticks.x;
        ticksY = gData.ticks.y;
        ticksZ = gData.ticks.z;
    } else {
        ticksX = gData.ticksLoading.x;
        ticksY = gData.ticksLoading.y;
        ticksZ = gData.ticksLoading.z;
    }


    let gXY = new PlaneGeometry(1, 1, ticksX.length - 1, ticksY.length - 1);
    ToQuads(gXY);
    let mXY = new LineBasicMaterial({color: 0xd6d6d6});
    let grXY = new LineSegments(gXY, mXY);
    grXY.scale.set(2000, 2000, 1);
    grXY.position.set(0, 0, -length);
    if (!visible) {
        grXY.material.visible = false;
    }
    gridsArr["xy" + "_" + type + "_" + scene.uuid] = grXY;
    scene.add(grXY);


    let gXZ = new PlaneGeometry(1, 1, ticksX.length - 1, ticksZ.length - 1);
    ToQuads(gXZ);
    let mXZ = new LineBasicMaterial({color: 0xd6d6d6});
    let grXZ = new LineSegments(gXZ, mXZ);
    grXZ.scale.set(2000, 2000, 1);
    grXZ.position.set(0, -length, 0);
    grXZ.rotation.x = Math.PI * -0.5;
    if (!visible) {
        grXZ.material.visible = false;
    }
    gridsArr["xz" + "_" + type + "_" + scene.uuid] = grXZ;
    scene.add(grXZ);


    let gZY = new PlaneGeometry(1, 1, ticksZ.length - 1, ticksY.length - 1);
    ToQuads(gZY);
    let mZY = new LineBasicMaterial({color: 0xd6d6d6});
    let grZY = new LineSegments(gZY, mZY);
    grZY.scale.set(2000, 2000, 1);
    grZY.position.set(-length, 0, 0);
    grZY.rotation.y = Math.PI * -0.5;
    if (!visible) {
        grZY.material.visible = false;
    }
    gridsArr["zy" + "_" + type + "_" + scene.uuid] = grZY;
    scene.add(grZY);


    const labelSize = 0.2;
    const size = length * 2;

    for (let i = 0; i <= ticksX.length; i = i + 2) {
        const tickDistance = size / (ticksX.length - 1);
        const xTick = -size / 2 + i * tickDistance;
        const xLabel = createText2D(ticksX[i] + "", "_xtick@_" + type, scene.uuid, "tick")
        if (ticksX[i] !== undefined) {
            xLabel.position.set(xTick - labelSize / 2, -50 - length - ticksX[i].toString().length * 15, 50 + length);
            xLabel.material.rotation = Math.PI / 2
            if (!visible) {
                xLabel.visible = false;
            }
            scene.add(xLabel);
        }
    }

    for (let i = 0; i <= ticksY.length; i = i + 2) {
        const tickDistance = size / (ticksY.length - 1);
        const yTick = -size / 2 + i * tickDistance;
        const yLabel = createText2D(ticksY[i] + "", "_ytick@_" + type, scene.uuid, "tick")
        if (ticksY[i] !== undefined) {
            yLabel.position.set(-length - 150, yTick - labelSize / 2, 50 + length);
            if (!visible) {
                yLabel.visible = false;
            }
            scene.add(yLabel);
        }

    }
    for (let i = 0; i <= ticksZ.length; i = i + 2) {
        const tickDistance = size / (ticksZ.length - 1);
        const zTick = -size / 2 + i * tickDistance;

        if (ticksZ[i] !== undefined) {
            const zLabel = createText2D(ticksZ[i] + "", "_ztick@_" + type, scene.uuid, "tick")
            zLabel.position.set(50 + length, -50 - length, zTick - labelSize / 2);
            if (!visible) {
                zLabel.visible = false;
            }
            scene.add(zLabel);
        }

    }
}


function initPlane(scene) {
    var colorValue = parseInt(layerCol.replace("#", "0x"), 16);
    var colored = new Color(colorValue);
    if (planeU[scene.uuid] !== undefined) {
        disposeMesh(planeU[scene.uuid], scene);
        planeU[scene.uuid] = undefined;
    }

    var renderer = Scatter.renderer();
    renderer.shadowMap.enabled = true;
    renderer.shadowMap.type = PCFSoftShadowMap;

    var geometry = new PlaneGeometry(7000, 7000, 32);
    var material = new MeshStandardMaterial({
        color: colored,
        side: DoubleSide
    });
    planeU[scene.uuid] = new Mesh(geometry, material);
    planeU[scene.uuid].userData = "floor";
    planeU[scene.uuid].receiveShadow = true;
    planeU[scene.uuid].position.set(0, -1500, 0);
    planeU[scene.uuid].lookAt(0, 0, 0);

    scene.add(planeU[scene.uuid]);

}

function updateAxes() {
    var opt = $('#axisOpt').val();
    var col = axisColorU;
    if (mainCheckBool) {
        generateAxes(Scatter.scene());
    } else {
        generateAxes(scene2);
    }

}

function updateCustomSelection() {
    var haloBool = $("#haloCheckbox").is(':checked');
    var clusterBool = $("#clusterCheckbox").is(':checked');
    var col = current_point_color;
    var colorValue = parseInt(col.replace("#", "0x"), 16);
    var coll = new Color(colorValue);

    var curType = current_module.split(/_(.+)/)[0];
    curType = curType.replace("overall", "meta");
    curType = curType.replace("targeted", "cluster");
    var curSelection = current_module.split(/_(.+)/)[1];
    current_module = curType + "_" + curSelection;
    updateCellColor(col, current_module);
    var curShape = $("#shapeOpt").val();
    var sizeval = parseInt($("#nsize").val());
    var row;

    row_selection_on_user_click = false;
    $('#metadg').datagrid('selectRow', curSelection);
    row = $('#metadg').datagrid('getRows')[curSelection];
    var curr_meta = row.name;
    highlightSelectedMeta(curr_meta, col, haloBool, $('#encasingType').val());

    changeShapeSelectedMeta(curr_meta, curShape);
    if (haloBool) {
        updateCellColor(current_halo_color, "metahalo_" + curSelection);
    } else {
        updateCellColor("#ffffff", "metahalo_" + curSelection);
    }

    row_selection_on_user_click = true;
    updateLabel();

    document.getElementById('clusterRow').style.display = "none";

    if (haloBool) {
        row.selectionSettings.haloBool = true;
        row.selectionSettings.haloColor = current_halo_color;
    } else {
        row.selectionSettings.haloBool = false;
    }

    if ($('#encasingType').val() !== "na") {
        row.selectionSettings.boundary = $('#encasingType').val();
        row.selectionSettings.boundaryColor = current_encasing_color;
    } else {
        row.selectionSettings.boundary = "na";
    }
    row.selectionSettings.nodeSize = sizeval;

    $("#customClusterOpt").val("NA");

}


function highlightCommunity(inx, meta) {
    var inx = inx + 1;
    var id_sbl_path = sub_modules[inx].split(";");
    var ids = id_sbl_path[3].split("->");
    var community = [];
    highlight_mode = 1;
    var nodes = [];
    var nodeids = [];
    if (!meta) {
        var coll = $("#module_" + inx).css("background-color");
        var a = coll.split("(")[1].split(")")[0];
        a = a.split(",");
        var col = convertToHex(a);
        if (col === "000000") {
            col = highlightColor;
        }
    } else {
        col = highlightColor;
    }
    Scatter.graphData().nodes.forEach(function (n) {
        if (ids.indexOf(n.id) !== -1) {
            if (!meta) {
                if (highlightType === "mixed") {
                    if (curr_mode !== "metasphere") {
                        colorNodeObj(n, col);
                        n.highlight = 1;
                    }
                    //n.color = highlightColor;

                } else {
                    highlightNode(n);
                }

                community.push({
                    id: n.id,
                    label: n.label
                });
            }
            nodes.push(n);
            nodeids.push(n.id);
        } else {
            if (!n.highlight === 0) { //other community
                n.color = greyColor;
            }
        }
    });

    if (meta) {
        addMetaSphereExperimental(nodes, inx);
    } else {
        var vertices = [];
        Scatter.graphData().nodes.forEach(function (n) {
            if (nodeids.indexOf(n.id) !== -1) {
                var pos = new Vector3();
                pos.x = n.x;
                pos.y = n.y;
                pos.z = n.z;
                vertices.push(pos);
            }

        });
        if (curr_mode !== "random" || Scatter.numDimensions() !== 2) {
            var sphere = new Sphere().setFromPoints(vertices);
            const distRatio = 1 + 300 / Math.hypot(sphere.center.x, sphere.center.y, sphere.center.z);
            Scatter.cameraPosition({
                x: sphere.center.x * distRatio,
                y: sphere.center.y * distRatio,
                z: sphere.center.z * distRatio
            }, // new position
                    {
                        x: 0,
                        y: 0,
                        z: 0
                    }, // lookAt ({ x, y, z })
                    2000 // ms transition duration
                    );
        }
        highlightDirectLinks();
    }

    current_fun_nodes = ids;
    displayCurrentSelectedNodes(community, "");
}

function resetCommunity(inx) {
    var id_sbl_path = sub_modules[inx].split(";");
    var ids = id_sbl_path[3].split("->");

    var count = 0;
    Scatter.graphData().nodes.forEach(function (n) {
        if (ids.indexOf(n.id) !== -1) {
            if (highlightType === "mixed") {
                n.highlight = 0;
                if (n.prevCol === undefined) {
                    colorNodeObj(n, n.tcolor);
                    n.color = n.tcolor;
                } else {
                    colorNodeObj(n, n.prevCol);
                    n.color = n.prevCol;
                }
            } else {
                unhighlightNode(n);
            }
        }
        if (n.highlight === 1) {
            count++;
        }
    });
    setEdgeColorGrey();
    $("#stats").empty();
}

function changeShapeCommunity(inx, shape) {
    var ids = boundingClustersGroup[inx].userData;
    Scatter.graphData().nodes.forEach(function (n) {
        if (ids.indexOf(n.id) !== -1) {
            shapeNodeObj(n, shape);
        }
    });
}

function changeShapeSelectedMeta(name, shape) {

    Scatter.graphData().nodes.forEach(function (n) {
        if (n.metatype === name) {
            n.highlight = 1;
            shapeNodeObj(n, shape);
        }
    });
}


function initScatter2() {

    nav_arr.forEach(function (n) {
        disposeMesh(n, scene2);
    });
    nav_arr = [];

    if (gData.edgesMain !== null && gData.edgesMain !== undefined) {
        gData.edgesMain.forEach(function (e) {
            scene2.add(e.__lineObj);
            nav_arr.push(e.__lineObj);
        });
    }
    gData.navigation.forEach(function (n) {
        var res = 0;
        if (n.tsize < 8) {
            res = 6;
        } else if (n.tsize < 10) {
            res = 12;
        } else {
            res = 16;
        }

        if (n.meta !== "mcia.seg") {
            if (["mcia", "procrustes"].indexOf(gData.reductionOpt) !== -1) {
                var geom = new SphereBufferGeometry(Math.cbrt(1) * 1.2 * 15, res, res / 2);

            } else {

                var geom = new SphereBufferGeometry(Math.cbrt(1) * 15, res, res / 2);
            }
            var col;
            //if (gData.navigation.length > 200 && gData.objects !== "NA") {
            //    col = boundingClusters[n.cluster].userData.tcolor
            //} else {
            col = n.colorb;
            //}
            var mat = new MeshPhongMaterial({
                color: col,
                transparent: true,
                opacity: 0.6,
                reflectivity: 0.5,
                emissive: 0x000000,
                depthWrite: false,
                depthTest: false
            });


            var mesh = new Mesh(geom, mat);

            if (n.tsize < 7.5) {
                mesh.visible = false;
            }

            sizeMeshObj(mesh, n.tsize * 0.1);
            mesh.position.set(n.fx, n.fy, n.fz);
            mesh.userData.nodeData = n;

            nav_arr.push(mesh);
            nav_nodes_arr.push(n);
            scene2.add(mesh);
        }
    });

    scene2.traverse(function (child) {
        if (child instanceof Mesh) {
            if (child.userData.nodeData !== undefined) {
                var colorValue;
                if (child.userData.nodeData.color !== undefined && child.userData.nodeData.color !== null) {
                    colorValue = parseInt(child.userData.nodeData.color.replace("#", "0x"), 16);
                }
                var col = new Color(colorValue);
                child.material.color = col;
                child.material.opacity = 0.8;
                child.visible = true;
            }
        }
    });
    initScene2 = false;
}

function sizeMeshObj(mesh, size) {
    const object = mesh;
    if (object !== null && object !== undefined) {
        object.scale.set(size, size, size);
    }
}

function floorToggle() {
    var checkBox = document.getElementById("floorOpt");
    var text = document.getElementById("text");
    if (checkBox.checked === true) {
        turnOffFloor(false);
    } else {
        turnOffFloor(true);
    }
}

function turnOffShadow(off) {
    var sceneArr = [Scatter.scene(), scene2];
    for (var i = 0; i < sceneArr.length; i++) {
        var sc = sceneArr[i];
        sc.traverse(function (child) {
            if (child instanceof Mesh) {
                if (child.userData.scaling !== undefined) {
                    if (off) {
                        child.castShadow = false;
                        child.receiveShadow = false;
                    } else {
                        child.castShadow = true;
                        child.receiveShadow = true;
                    }
                }
            }
        });
    }
}


function turnOffFloor(off) {
    var sceneArr = [Scatter.scene(), scene2];
    for (var i = 0; i < sceneArr.length; i++) {
        var sc = sceneArr[i];
        sc.traverse(function (child) {
            if (child instanceof Mesh) {
                if (child.userData === "floor") {
                    if (off) {
                        child.visible = false;
                    } else {
                        child.visible = true;
                    }
                }
            }
        });
    }
}

function turnOffWall(off) {
    var sceneArr = [Scatter.scene(), scene2];
    for (var i = 0; i < sceneArr.length; i++) {
        var sc = sceneArr[i];
        sc.traverse(function (child) {
            if (child instanceof Mesh) {
                if (child.userData === "wall") {
                    if (off) {
                        child.visible = false;
                    } else {
                        child.visible = true;
                    }
                }
            }
        });
    }
}

function colorNodeObjScene2(object, color) {
    object.userData.nodeData.color = color.replace("0x", "#");
    var colorValue = parseInt(color.replace("#", "0x"), 16);
    var col = new Color(colorValue);
    if (object !== null && object !== undefined) {
        const clonedMaterial = object.material.clone();
        clonedMaterial.setValues({
            color: col
        });
        object.material = clonedMaterial;
    }
}


function changePlaneType() {
    var mainCheck = document.getElementById("mainCheck");
    var sc;
    if (mainCheckBool) {
        sc = Scatter.scene();
    } else {
        sc = scene2;
    }
    initPlane(sc);
}

function searchNavigationScatter(id) {

    scene2.traverse(function (child) {
        if (child instanceof Mesh) {
            if (child.userData.nodeData !== undefined) {
                if (child.userData.nodeData.id === id || child.userData.nodeData.label === id) {
                    //shiftCameraToNode(child.userData.nodeData);
                    highlightNavigationNode(id);
                }
            }
        }
    });
}

function highlightNavigationNode(id) {
    scene2.traverse(function (child) {
        if (child instanceof Mesh) {
            if (child.userData.nodeData !== undefined) {
                if (child.userData.nodeData.id === id || child.userData.nodeData.label === id) {
                    child.userData.nodeData.highlight = 1;
                    var colorValue = parseInt(highlightColor.replace("#", "0x"), 16);
                    var col = new Color(colorValue);
                    child.material.color = col;
                    child.material.opacity = 0.6;
                    child.visible = true;
                } else {
                    //if(child.userData.nodeData.highlight !== 1){
                    child.material.transparent = true;
                    child.material.opacity = 0.15;
                    //}
                    var colorValue = parseInt("0xd3d3d3".replace("#", "0x"), 16);
                    var col = new Color(colorValue);
                    child.material.color = col;

                }

            }
        }
    });
}

function unhighlightNavigationNode(id) {
    scene2.traverse(function (child) {
        if (child instanceof Mesh) {
            if (child.userData.nodeData !== undefined) {
                if (child.userData.nodeData.id === id) {
                    var colorValue = parseInt(child.userData.nodeData.colorb.replace("#", "0x"), 16);
                    var col = new Color(colorValue);
                    child.material.color = col;
                    child.material.opacity = 0.6;
                } else {
                    if (nav_nodes_arr.length > 500) {
                        child.material.transparent = false;
                        child.material.opacity = 0.6;
                        var colorValue = parseInt(child.userData.nodeData.colorb.replace("#", "0x"), 16);
                        var col = new Color(colorValue);
                        child.material.color = col;
                    }
                }
            }
        }
    });
}

function deleteClusteringShapes(scene) {
    var nms = Object.keys(boundingClusters);
    for (var i = 0; i < nms.length; i++) {
        var nm = nms[i];
        deleteMetaSphere(nm, scene);
    }
}

function selectClusterScene2(e) {
    if (!e.focused) {
        e.focused = true;
        var arr = [];
        arr = arr.concat(e.ids);

        scene2.traverse(function (child) {
            if (child instanceof Mesh) {
                if (child.userData.nodeData !== undefined) {
                    if (arr.indexOf(child.userData.nodeData.id) !== -1) {
                        var colorValue = parseInt(highlightColor.replace("#", "0x"), 16);
                        var col = new Color(colorValue);
                        child.material.color = col;
                        child.material.opacity = 1;
                    }
                }
            }
        });

    } else {
        e.focused = false;
        var arr = [];
        arr = arr.concat(e.ids);

        scene2.traverse(function (child) {
            if (child instanceof Mesh) {
                if (child.userData.nodeData !== undefined) {
                    if (arr.length > 0) {
                        if (arr.indexOf(child.userData.nodeData.id) !== -1) {
                            var colorValue = parseInt(child.userData.nodeData.colorb.replace("#", "0x"), 16);
                            var col = new Color(colorValue);
                            child.material.color = col;
                            child.material.opacity = 1;
                        }
                    } else {
                        var colorValue = parseInt(child.userData.nodeData.colorb.replace("#", "0x"), 16);
                        var col = new Color(colorValue);
                        child.material.color = col;
                        child.material.opacity = 1;
                    }
                }
            }
        });


        Scatter.graphData().nodes.forEach(function (nd) {
            if (arr.length > 0) {

                if (arr.indexOf(nd.id) !== -1) {
                    // nd.highlight = 1
                    colorNodeObj(nd, nd.tcolor);
                    opaNodeObj(nd, 1);
                    deleteOutline(nd);
                    if (nd.outline === -1) {
                        //outlineNode(nd, highlightColor)
                    }
                } else {

                }
            } else {
                /*
                 //nd.highlight = 1
                 colorNodeObj(nd, nd.tcolor)
                 opaNodeObj(nd, 1)
                 deleteOutline(nd)
                 if (nd.outline === -1) {
                 //outlineNode(nd, highlightColor)
                 }*/
            }
        });
    }
}


function selectClusterNew(e) {
    if (!e.focused) {
        e.focused = true;
        var i = 0;
        var n;
        //var narr = e.userData.ids
        var arr = [];
        //for (var i = 0; i < cluster_arr.length; i++) {
        //var obj = cluster_arr[i];
        //if (obj.focused) {
        arr = e.ids;
        //}
        //}

        Scatter.graphData().nodes.forEach(function (nd) {
            if (arr.indexOf(nd.id) === -1) {

            } else {
                //nd.highlight = 1
                colorNodeObj(nd, highlightColor);
                opaNodeObj(nd, 1);
                if (nd.labeled === 0) {
                    addLabel(nd);
                }
                if (nd.outline === -1) {
                    //outlineNode(nd, highlightColor)
                }
            }
        });

    } else {
        e.focused = false;
        var arr = [];
        //for (var i = 0; i < cluster_arr.length; i++) {
        //var obj = cluster_arr[i];
        //if (obj.focused) {
        arr = e.ids;
        //}
        //}


        Scatter.graphData().nodes.forEach(function (nd) {
            if (arr.length > 0) {
                if (arr.indexOf(nd.id) !== -1) {
                    // nd.highlight = 1
                    colorNodeObj(nd, nd.tcolor);
                    opaNodeObj(nd, 1);
                    deleteOutline(nd);
                    if (nd.outline === -1) {
                        //outlineNode(nd, highlightColor)
                    }
                } else {

                }
            } else {
                /*
                 //nd.highlight = 1
                 colorNodeObj(nd, nd.tcolor)
                 opaNodeObj(nd, 1)
                 deleteOutline(nd)
                 if (nd.outline === -1) {
                 //outlineNode(nd, highlightColor)
                 
                 }*/
            }
        });

    }
}


function computeAlpha(scene) {
    shape_type = "ellipse";
    var nms = Object.keys(boundingClusters);
    for (var i = 0; i < nms.length; i++) {
        var nm = nms[i];
        deleteMetaSphere(nm, scene);
    }
    var col;
    var mesh;
    for (var i = 0; i < gData.ellipse.length; i++) {
        var ob = gData.ellipse[i].vb;
        var points = [];
        var xArr = [];
        var yArr = [];
        var zArr = [];
        for (var j = 0; j < ob[0].length; j++) {
            points.push(v(ob[0][j] * 1000, ob[1][j] * 1000, ob[2][j] * 1000));
        }
        var geom = new ConvexGeometry(points);
        var opp = 0.7;
        opp = opp + 0.3;
        var tr = true;
        if (opp > 0.7) {
            opp = 0.7;
            col = 0x431c53;
        } else {
            if (opp > 0.6) {
                opp = 1;
                col = 0xDA70D6;
            } else {
                opp = 0.25;
                col = 0xDA70D6;
            }
            tr = true;
        }
        var color = Set2[i];
        var material = new MeshPhongMaterial({
            color: color,
            transparent: true,
            alphaTest: 0.1,
            opacity: 0.4
        });
        mesh = new Mesh(geom, material);
        var nds = Scatter.graphData().nodes;
        var nodes_vec = [];
        nds.forEach(function (n) {
            if (n.cluster === i + 1) {
                nodes_vec.push(n.id);
                xArr.push(n.fx);
                yArr.push(n.fy);
                zArr.push(n.fz);
            }
        });

        var meanX = meanF(xArr);
        var meanY = meanF(yArr);
        var meanZ = meanF(zArr);
        var meanXYZ = v(meanX, meanY, meanZ);
        //geom.mergeVertices();
        //geom.computeVertexNormals()
        var clusterGroup = new Object3D();
        mesh.userData.ids = nodes_vec;
        mesh.userData.centroid = meanXYZ;
        mesh.userData.shadow = true;
        mesh.castShadow = false;
        mesh.receiveShadow = false;
        //mesh.scale.set(1000, 1000, 1000)
        mesh.focused = false;
        var colorValue = parseInt(color.replace("#", "0x"), 16);
        var colored = new Color(colorValue);
        mesh.userData.tcolor = colored;
        mesh.userData.tcolorhex = color;
        mesh.userData.opacity = opp;
        clusterGroup.userData = nodes_vec;
        clusterGroup.centroid = meanXYZ;
        //mesh.scale.set(1000, 1000, 1000)
        mesh.position.set(meanX, meanY, meanZ);
        clusterGroup.focused = false;
        clusterGroup.add(mesh);
        scene.add(clusterGroup);
        boundingClusters[i + 1] = mesh;
        boundingClustersGroup[i + 1] = clusterGroup;
    }
    var sceneCenter = v(0, 0, 0);
    for (var propertyNm in boundingClustersGroup) {
        var obj = boundingClustersGroup[propertyNm];
        obj.clusterNum = propertyNm;
        var clusterCenter = obj.centroid;
        var dir = new Vector3(); // create once an reuse it
        dir.subVectors(clusterCenter, sceneCenter).normalize();

        // scalar to simulate speed
        var speed = 0.05;
        var vector = dir.multiplyScalar(1000, 1000, 1000);
        obj.explosionDirection = vector;
        obj.targetPosition = new Vector3();
        obj.targetPosition.x += obj.explosionDirection.x;
        obj.targetPosition.y += obj.explosionDirection.y;
        obj.targetPosition.z += obj.explosionDirection.z;
        obj.targetPositionCopy = JSON.parse(JSON.stringify(obj.targetPosition));

        Scatter.graphData().nodes.forEach(function (n) {
            if (obj.userData.indexOf(n.id) !== -1) {
                //n.explosionDirection = obj.explosionDirection
                //obj.add(n.__threeObj)
            }
        });
    }
}


function deleteOutlineScene2(object) {
    var n = object.userData.nodeData;
    if (outlineUIs[n.id] !== undefined) {
        var group = object;
        for (var i = 0; i < group.children.length; i++) {
            var child = group.children[i];
            if (child.userData.type === "halo") {
                group.remove(child);
            }
        }

        //group.remove(group.children[group.children.length - 1]);
        var mesh = outlineUIs[n.id];
        scene2.remove(mesh);
        if (mesh) {
            if (mesh.material.geometry) {
                mesh.geometry.dispose();
                mesh.geometry = undefined;
            }
            if (mesh.material.map)
                mesh.material.map.dispose();
            if (mesh.material.lightMap)
                mesh.material.lightMap.dispose();
            if (mesh.material.bumpMap)
                mesh.material.bumpMap.dispose();
            if (mesh.material.normalMap)
                mesh.material.normalMap.dispose();
            if (mesh.material.specularMap)
                mesh.material.specularMap.dispose();
            if (mesh.material.envMap)
                mesh.material.envMap.dispose();
            mesh.material.dispose();
            mesh.material = undefined;
            mesh = undefined;
            outlineUIs[n.id] = null;
            delete outlineUIs[n.id];
        }
    }
}


function outlineNodeScene2(object, color) {
    object.userData.nodeData.color = color.replace("0x", "#");
    var n = object.userData.nodeData;
    var colorValue = parseInt(color.replace("#", "0x"), 16);
    var col = new Color(colorValue);
    if (object !== null && object !== undefined) {
        const clonedMaterial = object.material.clone();
        clonedMaterial.setValues({
            color: col
        });
        object.material = clonedMaterial;
        if (n.outline === 1) {
            deleteOutlineScene2(n);
        }
        n.outline = 1;
        var particle = new Sprite(new SpriteMaterial({
            map: new CanvasTexture(generateSprite(color)),
            alphaTest: 0.5,
            transparent: true,
            fog: true
        }));
        particle.scale.x = particle.scale.y = object.userData.scaling[0] * 3 + 115;
        var jsonKey = n.id;
        outlineUIs[jsonKey] = particle;
        outlineUIs[jsonKey].userData.type = "halo";
        outlineUIs[jsonKey].userData.id = n.id;
        outlineUIs[jsonKey].userData.color = color.replace("#", "0x");
        //particle.position.set(threeNodes[n.id].position.x, threeNodes[n.id].position.y, threeNodes[n.id].position.z)

        object.add(particle);
    }
}

function mergeClusters() {
    var rows = $('#mdg').datagrid('getChecked');
    if (rows.length < 2) {
        $.messager.alert('Error', 'At least two rows should be selected before using the merging function.', 'error');
        return;
    }
    var inxArr = [];
    var idsArr = [];
    var nmsArr = [];
    for (var i = 0; i < rows.length; i++) {
        inxArr.push(rows[i].index);
        idsArr.push(...rows[i].ids);
        nmsArr.push(rows[i].name);
    }

    var groupVal = 1;

    var i = cluster_arr.length;
    while (i--) {
        if (cluster_arr[i].group === "Merged") {
            groupVal++;
        }
        if (inxArr.indexOf(i) !== -1) {
            cluster_arr.splice(i, 1);
        }
    }

    newlyMergedRow = {
        index: 0,
        ids: idsArr,
        group: "Merged",
        name: "Group " + groupVal,
        size: idsArr.length,
        color: '<span id=\"meta_' + 0 + '\" style="background-color:#ffffff" onclick="openModuleColorPicker(this);event.stopPropagation()">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp</span>',
        colorhalo: '<span id=\"metahalo_' + 0 + '\" style="background-color:#ffffff" onclick="openModuleColorPicker(this);event.stopPropagation()">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp</span>',
        child: nmsArr,
        groupVal: groupVal
    };
    cluster_arr.unshift(newlyMergedRow);

    for (var i = 0; i < cluster_arr.length; i++) {
        cluster_arr[i].index = i;
    }
    ;
    $('#mdg').datagrid('loadData', []);
    $('#mdg').datagrid('loadData', cluster_arr);
}

function deleteMesh(mesh, scene) {
    scene.remove(mesh);
    if (mesh.geometry) {
        mesh.geometry.dispose();
        mesh.geometry = undefined;
    }
    if (mesh.material) {
        if (mesh.material.map)
            mesh.material.map.dispose();
        if (mesh.material.lightMap)
            mesh.material.lightMap.dispose();
        if (mesh.material.bumpMap)
            mesh.material.bumpMap.dispose();
        if (mesh.material.normalMap)
            mesh.material.normalMap.dispose();
        if (mesh.material.specularMap)
            mesh.material.specularMap.dispose();
        if (mesh.material.envMap)
            mesh.material.envMap.dispose();
        mesh.material.dispose();
        mesh.material = undefined;
    }
    mesh = undefined;


}


function addTextNodeObj(n, sceneInt, type) { // 98 scene1/ 99 scene2
    const object = n.__threeObj;
    if (object !== null && object !== undefined) {
        var sprite;
        var text = "";
        if (type === "name") {
            text = n.label;
        } else if (type === "meta") {
            text = n.meta;
        } else {
            text = n.meta2;
        }
        sprite = new SpriteText(text);
        sprite.color = n.color;
        sprite.textHeight = 30;
        sprite.fontSize = sceneInt;
        sprite.userData === "textAsNode";
        sprite.material.transparent = false;
        object.add(sprite);
    }
}

function addTextObj(object, sceneInt) { // 98 scene1/ 99 scene2
    if (object !== null && object !== undefined) {
        const sprite = new SpriteText(object.userData.nodeData.label);
        sprite.color = object.userData.nodeData.color;
        sprite.textHeight = 30;
        sprite.fontSize = sceneInt;
        sprite.userData === "textAsNode";
        sprite.material.transparent = false;
        object.add(sprite);
    }
}

function removeTextNodeObj(object) {
    if (object !== null && object !== undefined) {
        object.traverse(function (child) {
            if (child._fontSize !== undefined) {
                if (child._fontSize === 48 || child._fontSize === 49) {
                    child.visible = false;
                    child.scale.set(1, 1, 1);
                    Scatter.scene().remove(child);
                }
            }
        });
    }
}

function setTextNodeSize(object, increaseBool) {
    if (object !== null && object !== undefined) {
        object.traverse(function (child) {
            if (child._fontSize !== undefined) {
                if (child._fontSize === 48 || child._fontSize === 49) {
                    if (increaseBool) {
                        child.scale.set(child.scale.x * 1.1, child.scale.y * 1.1, child.scale.z * 1.1);
                    } else {
                        child.scale.set(child.scale.x * 0.9, child.scale.y * 0.9, child.scale.z * 0.9);
                    }
                }
            }
        });
    }
}


function testEnrichment() {
    doEnrichmentTests(function (result) {
        if (result === 'NA.json') {
            $.messager.alert('Enrichment Error', 'No result has been found for the queried molecules in our database. Please try with another database or other molecules!', 'error');
        } else if ($('#enrichdb').val() === "integ") {
            loadEnrichTable(result);
        } else {
            $.getJSON(usr_dir + '/' + result, function (raw_data) {
                $('#dge').datagrid({
                    columns: [
                        [{
                                field: 'pathname',
                                title: 'Name',
                                width: 160
                            },
                            {
                                field: 'hit',
                                title: 'Hits',
                                width: 40
                            },
                            {
                                field: 'pval',
                                title: 'P-val',
                                width: 60
                            },
                            {
                                field: 'padj',
                                title: 'P-val(adj.)',
                                width: 60
                            },
                            {
                                field: 'color',
                                title: 'Color',
                                width: 40
                            }
                        ]
                    ]
                });
                currentEnrichFile = result.substring(0, result.length - 5);
                focus_enr_anot = raw_data['fun.anot'];
                var fun_hit = raw_data['hit.num'];
                var fun_pval = raw_data['fun.pval'];
                var fun_padj = raw_data['fun.padj'];
                var data_grid = $('#dge');
                //empty if there is any
                data_grid.datagrid('loadData', {
                    "total": 0,
                    "rows": []
                });
                var mdl_rows = [];
                var idx = 0;
                $.each(focus_enr_anot, function (k, v) {
                    mdl_rows.push({
                        pathname: k,
                        hit: fun_hit[idx],
                        pval: fun_pval[idx],
                        padj: fun_padj[idx],
                        color: '<span id=\"function_' + idx + '\">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp</span>'
                    });
                    idx = idx + 1;
                });

                data_grid.datagrid({
                    onSelect: function (index, row) {
                        var nodeIDs = focus_enr_anot[row.pathname];
                        current_fun_nodes = nodeIDs;
                        highlightFunEnrichNodes(nodeIDs, row.pathname);
                        updateCellColor(highlightColor, "function_" + index);
                        if (current_main_plot === "score") {
                            addArrows(nodeIDs);
                        }
                    },
                    onUnselect: function (index, row) {
                        var nodeIDs = focus_enr_anot[row.pathname];
                        current_fun_nodes = null;
                        unHighlightFunEnrichNodes(nodeIDs);
                        updateCellColor("#ffffff", "function_" + index);
                        if (current_main_plot === "score") {
                            deleteArrows(nodeIDs);
                        }
                    }
                }).datagrid('loadData', mdl_rows);
            });
        }
        $.messager.progress('close');
    });
}

var comp_res = [];

function doEnrichmentTests(callBack) {

    var fundb = $('#enrichdb').val();

    if (fundb === "NA") {
        $.messager.alert('Error', 'Enrichment analysis can not be performed on unannotated data!', 'error');
        return;
    }

    if (comp_res.length === 0) {
        $.messager.alert('', 'Please perform comparison test before performing enrichment analysis!', 'info');
        return;
    }

    var node_ids = "";
    comp_res.forEach(function (n) {
        node_ids = node_ids + '; ' + n.id;
    });

    node_ids = node_ids.substring(2, node_ids.length);
    var enrFunc = 'scatterEnrichment';
    //if (fundb === "keggm" || fundb === "integ") {
    //    enrFunc = 'keggEnrichment'
    //}
    $.ajax({
        beforeSend: function () {
            $.messager.progress({
                text: 'Processing .....'
            });
        },
        dataType: "html",
        type: "POST",
        url: '/OmicsAnalyst/faces/AjaxCall',
        data: {
            function: enrFunc,
            IDs: node_ids,
            funDB: fundb
        },
        async: true,
        cache: false,
        success: function (result) {
            return callBack(result);
        },
        error: function () {
            $.messager.alert('Error', 'Failed to process the request!', 'error');
            $.messager.progress('close');
        }
    });

}

function setupClusteringOpts() {
    var qOpts = $('#metaopt');
    //removeOptions(qOpts);

    var count = 0;
    var myOpts = '';
    for (var propNm in gData.meta) {
        if (count === 0) {
            curr_meta = propNm;
        }
        if (propNm !== "newcolumn" && propNm !== "cluster") {
            myOpts = myOpts + '<option value="' + propNm + '">' + propNm + '</option>';
            count++;
        }
    }
    var qOpts2 = $('#biplotMetaOpt');
    //removeOptions(qOpts);
    qOpts2.empty();
    var qOpts3 = $('#encasingMetaOpt');
    qOpts3.empty();
    var count = 0;
    var myOpts2 = "";
    var myOpts3 = "";
    for (var propNm in gData.meta) {
        if (propNm !== "newcolumn") {
            myOpts2 = myOpts2 + '<option value="' + propNm + '">' + propNm + '</option>';
            myOpts3 = myOpts3 + '<option value="' + propNm + '">' + propNm + '</option>';
        }
    }
    qOpts2.append(myOpts2);
    qOpts3.append(myOpts3);

    qOpts.append(myOpts);

    $('#metadg').datagrid({
        onSelect: function (index, row) {
            comp_last_selected_table = "overall";
            loadTargetedOpts(row.name);

            /*
             if (row_selection_on_user_click) {
             var ids_arr = row.ids
             Scatter.graphData().nodes.forEach(function (n) {
             if (ids_arr.indexOf(n.id) !== -1) {
             colorNodeObj(n, highlightColor)
             }
             });
             updateCellColor(highlightColor, "meta_" + index);
             }*/
            if (row.name.includes("Cluster")) {
                displayPatternData(row, "cluster");
                updateMetaInfo(index, row);
            } else {
                displayPatternData(row, "metatype");
                //updateCustomInfo(index, row);
                updateMetaInfo(index, row);
            }
        },

        onUnselect: function (index, row) {
            //current_selected_row = "";
            //resetCommunity(index);
            var ids_arr = row.ids;
            Scatter.graphData().nodes.forEach(function (n) {
                if (ids_arr.indexOf(n.id) !== -1) {
                    colorNodeObj(n, n.tcolor);
                }
            });
            current_fun_nodes = null;
            var e = cluster_arr[index];
            if (e !== undefined) {
                cluster_arr[index].focused = true;
                selectClusterNew(e);
            }
            updateCellColor(row.origColor, "meta_" + index);
        }
    });
    if (gData.meta.omics !== undefined) {
        var arr = gData.meta[curr_meta];

        var setobj = {};
        for (var i = 0; i < arr.length; i++) {
            if (setobj[arr[i]] === undefined) {
                setobj[arr[i]] = 1;
            } else {
                setobj[arr[i]]++;
            }
        }

        if (current_main_plot === "score") {
            var nodes = Scatter.graphData().nodes;
            for (var i = 0; i < nodes.length; i++) {
                var n = nodes[i];
                n.type = gData.meta["omics"][i];
            }
        }
    }
    curr_meta = $('#metaopt').val();
    loadMeta();
}

function setOverallGeneEnrichDbs(org) {
    var qOpts = $('#enrichdb');
    qOpts.empty();
    var sharedOpts = '<optgroup label="--Metabolites--" style="padding-left: 20px;">' +
            '<option value="keggm" style="padding-left: 30px;">KEGG (Met.)</option>' +
            '</optgroup>';
    if (gData.ko === "ko") {
        qOpts.append('<option value="kegg">KEGG</option>');
    } else if (org === "hsa" | org === "mmu") { //Reactome, PANTHER and Motif
        qOpts.append(
                '<optgroup label="--Genes and Proteins--" style="padding-left: 20px;">' +
                '<option value="kegg">KEGG (gene)</option>' +
                '<option value="reactome">Reactome</option>' +
                '<option value="go_bp">GO:BP</option>' +
                '<option value="go_mf">GO:MF</option>' +
                '<option value="go_cc">GO:CC</option>' +
                '<option value="panthbp">PANTHER:BP</option>' +
                '<option value="panthmf">PANTHER:MF</option>' +
                '<option value="panthcc">PANTHER:CC</option>' +
                '<option value="motif">Motif</option>' +
                '<option value="tissue">Tissue Marker</option>' +
                '<option value="cell">Cell Marker</option>' +
                '</optgroup>' +
                sharedOpts
                );
    } else if (org === "cel" | org === "dme") { //Reactome
        qOpts.append(
                '<optgroup label="--Gene and Protein only--" style="padding-left: 20px;">' +
                '<option value="kegg">KEGG (gene)</option>' +
                '<option value="reactome">Reactome</option>' +
                '<option value="go_bp">GO:BP</option>' +
                '<option value="go_mf">GO:MF</option>' +
                '<option value="go_cc">GO:CC</option>' +
                '<option value="panthbp">PANTHER:BP</option>' +
                '<option value="panthmf">PANTHER:MF</option>' +
                '<option value="panthcc">PANTHER:CC</option>' +
                '</optgroup>' +
                sharedOpts
                );
    } else if (org === "dre" | org === "gga" | org === "bta" | org === "rno") {
        qOpts.append(
                '<optgroup label="--Gene and Protein only--" style="padding-left: 20px;">' +
                '<option value="kegg">KEGG (gene)</option>' +
                '<option value="go_bp">GO:BP</option>' +
                '<option value="go_mf">GO:MF</option>' +
                '<option value="go_cc">GO:CC</option>' +
                '<option value="panthbp">PANTHER:BP</option>' +
                '<option value="panthmf">PANTHER:MF</option>' +
                '<option value="panthcc">PANTHER:CC</option>' +
                '</optgroup>' +
                sharedOpts
                );
    } else {
        qOpts.append('<option value="NA">None</option>');
    }

}


var currentEnrichFile = "";

function loadEnrichTable(result) {
    $.getJSON(usr_dir + '/' + result, function (raw_data) {
        currentEnrichFile = result.substring(0, result.length - 5);
        var data_grid = $('#dge');
        if (raw_data['fun.pval2'] !== undefined) {

            data_grid.datagrid({
                columns: [
                    [{
                            field: 'pathname',
                            title: 'Name',
                            width: 160
                        },
                        {
                            field: 'hit1',
                            title: 'Hits(G/M)',
                            width: 50
                        },
                        {
                            field: 'pval2',
                            title: 'P-val(joint)',
                            width: 70
                        },
                        {
                            field: 'color',
                            title: 'Color',
                            width: 40
                        }
                    ]
                ]
            });
            var fun_hit1 = raw_data['hit.num1'];
            var fun_hit2 = raw_data['hit.num2'];
            var fun_pval1 = raw_data['fun.pval1'];
            var fun_pval2 = raw_data['integ.pval'];
            var path_anot = raw_data['fun.anot'];
            for (var propertyName in path_anot) {
                if (propertyName === "KO") {
                    delete path_anot[propertyName];
                }
            }
            focus_fun_anot_kos = path_anot;
            //empty if there is any
            data_grid.datagrid('loadData', {
                "total": 0,
                "rows": []
            });
            var mdl_rows = [];
            var idx = 0;
            $.each(path_anot, function (k, v) {
                mdl_rows.push({
                    pathname: k,
                    //hit1: fun_hit2[idx] + "/" + fun_hit1[idx],
                    hit1: fun_hit2[idx],
                    pval2: fun_pval2[idx],
                    color: '<span style=\"background-color:#ffffff\" id=\"function_' + idx + '\">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp</span>'
                });
                idx = idx + 1;
            });
        } else {
            data_grid.datagrid({
                columns: [
                    [{
                            field: 'pathname',
                            title: 'Name',
                            width: 160
                        },
                        {
                            field: 'hit',
                            title: 'Hits',
                            width: 40
                        },
                        {
                            field: 'pval',
                            title: 'P-val',
                            width: 60
                        },
                        {
                            field: 'padj',
                            title: 'P-val(adj.)',
                            width: 60
                        } //,
                        // {field: 'color', title: 'Color', width: 40}
                    ]
                ]
            });
            //focus_fun_anot_kos = raw_data['hits.query'];
            //focus_fun_anot_edges = raw_data['hits.edge'];
            var fun_hit = raw_data['hit.num'];
            var fun_pval = raw_data['fun.pval'];
            var fun_padj = raw_data['fun.padj'];
            var path_anot = raw_data['fun.anot'];
            for (var propertyName in path_anot) {
                if (propertyName === "KO") {
                    delete path_anot[propertyName];
                }
            }
            focus_fun_anot_kos = path_anot;

            var data_grid = $('#dge');
            //empty if there is any
            data_grid.datagrid('loadData', {
                "total": 0,
                "rows": []
            });
            var mdl_rows = [];
            var idx = 0;
            $.each(path_anot, function (k, v) {
                mdl_rows.push({
                    pathname: k,
                    hit: fun_hit[idx],
                    pval: fun_pval[idx],
                    padj: fun_padj[idx],
                    color: '<span style=\"background-color:#ffffff\" id=\"function_' + idx + '\">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp</span>'
                });
                idx = idx + 1;
            });
        }



        data_grid.datagrid({
            onSelect: function (index, row) {
                current_fun_kos = focus_fun_anot_kos[row.pathname];
                if (typeof current_fun_kos === "string") {
                    current_fun_kos = [current_fun_kos];
                }
                highlightFunEnrichNodes(current_fun_kos, row.pathname);
                updateCellColor(highlightColor, "function_" + index);
            },
            onUnselect: function (index, row) {
                var arr = focus_fun_anot_kos[row.pathname];
                current_fun_kos = null;
                unHighlightFunEnrichNodes(arr, row.pathname);

                updateCellColor("#ffffff", "function_" + index);
                var stats = $("#stats");
                stats.empty();
                //resetNetworkk();
            }
        }).datagrid('loadData', mdl_rows);
    });
}

function setEdgeColorAsNode() {
    if (gData.links.length === 0) {
        $.messager.alert('Error', 'No edges are detected within the scatter plot!', 'error');
        return;
    }
    if (current_main_plot === "score") {
        Scatter.graphData().links.forEach(function (l) {
            colorLineObj2(l, l.color);
        });
    } else {

    }
}

function getColorArr(clusters) {
    var propNms = Object.keys(clusters);
    $.ajax({
        beforeSend: function () {
            $.messager.progress({
                text: 'Processing .....'
            });
        },
        dataType: "html",
        type: "POST",
        url: '/OmicsAnalyst/faces/AjaxCall',
        data: {
            function: 'generateColorArray',
            number: propNms.length
        },
        async: false,
        success: function (result) {
            $.messager.progress('close');
            $.getJSON(usr_dir + "/" + result, function (jsonDat) {
                var colArr = jsonDat;
                if (current_main_plot === "score") {
                    var nodes = Scatter.graphData().nodes;
                    setTimeout(function (n) {
                        for (var i = 0; i < nodes.length; i++) {
                            var n = nodes[i];
                            var inx = propNms.indexOf(n.cluster + "");
                            if (inx !== -1) {

                                n.colorb = colArr[inx];
                                n.tcolor = colArr[inx];
                                colorNodeObj(n, colArr[inx]);
                            } else {
                                sizeNodeObj(n, 0.01);
                            }
                        }
                    }, 1000);

                } else {
                    scene2.traverse(function (child) {
                        if (child instanceof Mesh) {
                            if (child.userData.nodeData !== undefined) {
                                var n = child.userData.nodeData;
                                var inx = propNms.indexOf(n.metatype);
                                if (inx !== -1) {
                                    colorNodeObj(n, colArr[inx]);
                                }
                            }
                        }
                    });
                }

                var mdl_rows = [];
                var idx = 0;

                for (var propNm in clusters) {
                    if (propNm !== "NA") {
                        const id = "overall_" + idx;
                        const colorId = "meta_" + idx;

                        mdl_rows.push({
                            edit_id: id,
                            color_id: colorId,
                            index: idx,
                            origColor: colArr[idx],
                            ids: clusters[propNm],
                            name: "Cluster " + (idx + 1),
                            size: clusters[propNm].length,
                            selectionSettings: {
                                nodeSize: 2,
                                haloBool: false,
                                haloColor: "#ffffff",
                                boundary: "na",
                                boundaryColor: "#ffffff"
                            },
                            color: '<span id=\"' + colorId + '\" style="background-color:' + colArr[idx] + '">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp</span>',
                            colorhalo: '<button class="icon-edit" id=\"' + id + '\" style="width:16px;height:16px;border:0"></button>'
                        });

                        idx++;
                    }
                }
                $('#metadg').datagrid('loadData', []);
                $('#metadg').datagrid('loadData', mdl_rows);

                cluster_arr = mdl_rows;
                var sel = document.getElementById("targetSelection");
                removeOptions(sel);
                var arr = $("#metadg").datagrid("getRows");
                for (var i = 0; i < arr.length; i++) {
                    var option = document.createElement("option");
                    option.text = arr[i].name;
                    option.value = i;
                    sel.add(option);
                    count++;

                    $('#' + arr[i].color_id).bind('click keypress', function (event) {

                        //openModuleColorPicker(this, arr[i].name);
                        event.stopPropagation();
                    });

                    $('#' + arr[i].edit_id).bind('click keypress', function (event) {

                        setSelection(this);
                        event.stopPropagation();
                    });
                }
                loadTargetedOpts();


            });
        }
    });
}

function sortObject(o) {
    var sorted = {},
            key, a = [];

    for (key in o) {
        if (o.hasOwnProperty(key)) {
            a.push(key);
        }
    }

    a.sort();

    for (key = 0; key < a.length; key++) {
        sorted[a[key]] = o[a[key]];
    }
    return sorted;
}

function performCustomClustering(selection) {

    var nodes_arr = [];
    var alg = $("#targetedOpts").val();
    if (alg === "NA") {
        return;
    }
    if (current_main_plot === "score") {
        var nodes = Scatter.graphData().nodes;

        for (var i = 0; i < nodes.length; i++) {
            var n = nodes[i];
            if (n.cluster === selection || n.metatype === selection) {
                if (nodes_arr.length === 0) {
                    nodes_arr = [n.id];
                } else {
                    nodes_arr = nodes_arr + "; " + n.id;
                }
            }
        }

    } else {
        scene2.traverse(function (child) {
            if (child instanceof Mesh) {
                if (child.userData.nodeData !== undefined) {
                    var n = child.userData.nodeData;
                    if (n.cluster === selection || n.metatype === selection) {
                        if (nodes_arr.length === 0) {
                            nodes_arr = [n.id];
                        } else {
                            nodes_arr = nodes_arr + "; " + n.id;
                        }
                    }
                }
            }
        });
    }

    $.ajax({
        beforeSend: function () {
            $.messager.progress({
                text: 'Processing .....'
            });
        },
        dataType: "html",
        type: "POST",
        url: '/OmicsAnalyst/faces/AjaxCall',
        data: {
            function: 'performCustomClustering',
            IDs: nodes_arr,
            alg: alg
        },
        async: false,
        success: function (result) {
            $.messager.progress('close');
            $.getJSON(usr_dir + "/" + result, function (jsonDat) {
                var minclust = parseInt($('#minclust').val());
                var clusters = {};
                var nodes = [];
                if (current_main_plot === "score") {
                    nodes = Scatter.graphData().nodes;
                } else {
                    nodes = [];
                    scene2.traverse(function (child) {
                        if (child instanceof Mesh) {
                            if (child.userData.nodeData !== undefined) {
                                nodes.push(child.userData.nodeData);
                            }
                        }
                    });
                }

                nodes.forEach(function (n) {
                    n.targetedcluster = "NA";
                });

                for (var j = 0; j < jsonDat.cluster.inxs.length; j++) {
                    var n = nodes[jsonDat.cluster.inxs[j] - 1];
                    n.targetedcluster = jsonDat.cluster.cluster[j];
                    //n.metatype = jsonDat.cluster.cluster[j];
                    if (clusters[jsonDat.cluster.cluster[j]] === undefined) {
                        clusters[jsonDat.cluster.cluster[j]] = [n.id];
                    } else {
                        clusters[jsonDat.cluster.cluster[j]].push(n.id);
                    }

                }

                var mdl_rows = [];
                var idx = 0;

                for (var propNm in clusters) {
                    mdl_rows.push({
                        index: idx,
                        group: selection,
                        ids: clusters[propNm],
                        name: "Cluster " + (idx + 1),
                        size: clusters[propNm].length,
                        selectionSettings: {
                            nodeSize: 2,
                            haloBool: false,
                            haloColor: "#ffffff",
                            boundary: "na",
                            boundaryColor: "#ffffff"
                        },
                        color: '<span id=\"cluster_' + idx + '\" style="background-color:#ffffff" onclick="openModuleColorPicker(this);event.stopPropagation()">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp</span>',
                        colorhalo: '<button class="icon-edit" id=\"targeted_' + idx + '\" style="width:16px;height:16px;border:0" onclick="setSelection(this);event.stopPropagation();"></button>'
                    });
                    idx++;
                }

                $('#mdg').datagrid('loadData', []);
                $('#mdg').datagrid('loadData', mdl_rows);
                initMdgFunctions();
                cluster_arr2 = mdl_rows;
            });
        }
    });
}

function loadTargetedOpts() {
    //document.getElementById('selectTargetId').innerHTML = "<b>" + name + "</b>";

    var metaVal = $("#metaopt").val();
    var sel = document.getElementById("targetedOpts");
    removeOptions(sel);
    var algOpts = ["kmeans", "peak", "meanshift"];
    var algOptsText = ["K-means", "Density Peak", "Mean Shift"];
    if (algOpts.indexOf(metaVal) === -1) {
        for (var i = 0; i < algOpts.length; i++) {
            var option = document.createElement("option");
            option.text = algOptsText[i];
            option.value = algOpts[i];
            sel.add(option);
        }
    } else {
        var count = 0;
        for (var propNm in gData.meta) {
            if (count === 0) {
                curr_meta = propNm;
            }
            var option = document.createElement("option");
            option.text = propNm;
            sel.add(option);
            count++;
        }
    }
}

function performTargetedClustering() {
    var metaVal = $("#metaopt").val();
    var currentSelection = $("#targetSelection").val();
    var currentRows = $("#metadg").datagrid("getRows");
    var currentRow = currentRows[parseInt(currentSelection)];

    var algOpts = ["kmeans", "peak", "meanshift"];
    if (algOpts.indexOf(metaVal) === -1) {
        performCustomClustering(currentRow.name);
    } else {
        performCustomMeta(currentRow);
    }
    highlightNodesAndHideAllOther(currentRow.ids);

}

function performCustomMeta(meta) {
    var nodes = Scatter.graphData().nodes;
    var targetedVal = $("#targetedOpts").val();
    for (var i = 0; i < nodes.length; i++) {
        var n = nodes[i];
        n.targetedcluster = gData.meta[targetedVal][i];
    }
    var clusters = {};
    for (var i = 0; i < nodes.length; i++) {
        var n = nodes[i];
        if (parseInt(n.cluster) === meta.index + 1) {
            if (clusters[n.targetedcluster] === undefined) {
                clusters[n.targetedcluster] = [n.id];
            } else {
                clusters[n.targetedcluster].push(n.id);
            }
        }
    }
    var mdl_rows = [];
    var idx = 0;
    for (var propNm in clusters) {
        mdl_rows.push({
            group: targetedVal,
            index: idx,
            ids: clusters[propNm],
            name: propNm,
            size: clusters[propNm].length,
            color: '<span id=\"cluster_' + idx + '\" style="background-color:#ffffff" onclick="openModuleColorPicker(this);event.stopPropagation()">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp</span>',
            colorhalo: '<button class="icon-edit "id=\"targeted_' + idx + '\" style="width:16px;height:16px;border:0" onclick="setSelection(this);event.stopPropagation();"></button>'
        });
        idx++;
    }

    $('#mdg').datagrid('loadData', []);
    $('#mdg').datagrid('loadData', mdl_rows);
    initMdgFunctions();
    cluster_arr2 = mdl_rows;
}



function initMdgFunctions() {
    $('#mdg').datagrid({
        onSelect: function (index, row) {
            comp_last_selected_table = "targeted";
            updateCustomInfo(index, row);
            if (row_selection_on_user_click) {
                var ids_arr = row.ids;
                Scatter.graphData().nodes.forEach(function (n) {
                    if (ids_arr.indexOf(n.id) !== -1) {
                        colorNodeObj(n, highlightColor);
                    }
                });
                updateCellColor(highlightColor, "cluster_" + index);
            }
            if (row.name.includes("Cluster")) {
                displayPatternData(row, "targetedcluster");
            } else {
                displayPatternData(row, "meta");
            }
        },
        onUnselect: function (index, row) {
            //current_selected_row = "";
            current_fun_nodes = null;
            var e = cluster_arr2[index];
            if (e !== undefined) {
                cluster_arr2[index].focused = true;
                selectClusterNew(e);
            }
            updateCellColor("#ffffff", "cluster_" + index);
        }
    });

}

function setSelection(element) {
    current_module = element.id;
    var curType = current_module.split(/_(.+)/)[0];
    curType = curType.replace("overall", "meta");
    curType = curType.replace("targeted", "cluster");
    var curIndex = current_module.split(/_(.+)/)[1];

    var row;
    if (curType === "meta") {
        row = $("#metadg").datagrid("getRows")[curIndex];
        $('#metadg').datagrid('selectRow', curIndex);
        //updateMetaInfo(curIndex, row)
    } else {
        row = $("#mdg").datagrid("getRows")[curIndex];
        $('#mdg').datagrid('selectRow', curIndex);
        //updateCustomInfo(curIndex, row)
    }
    $("#nodeSliderSize").val(parseInt(row.selectionSettings.nodeSize));
    $("#nsize").val(parseInt(row.selectionSettings.nodeSize));
    document.getElementById("haloCheckbox").checked = row.selectionSettings.haloBool;
    $("#customHalo").spectrum("set", row.selectionSettings.haloColor);
    $("#encasingType").val(row.selectionSettings.boundary);
    //$("#customEncasing").spectrum("set", row.selectionSettings.boundaryColor);
    if ($('#viewFocusOpt').val() !== "score") {
        $.messager.alert('Error', 'Please first update the current View Type to Scores!', 'error');
        return;
    } else {
        $('#highlightdlg').dialog('open');
    }
}

function highlightNodesAndHideAllOther(ids) {
    var nodes;
    if (current_main_plot === "score") {
        nodes = Scatter.graphData().nodes;
        nodes.forEach(function (n) {
            if (ids.indexOf(n.id) === -1) {
                opaNodeObj(n, 0.2);
            } else {
                opaNodeObj(n, 0.8);
            }
        });
    } else {
        scene2.traverse(function (child) {
            if (child instanceof Mesh) {
                if (ids.indexOf(child.userData.nodeData.id) === -1) {
                    child.material.transparent = false;
                    child.material.opacity = 0.2;
                } else {
                    child.material.transparent = false;
                    child.material.opacity = 0.8;
                }
            }
        });
    }
}

function deleteSpherical(sceneType) {
    if (sceneType === "main") {
        var scene = Scatter.scene();
        var sphereMesh = sphereMesh1;
    } else {
        var scene = scene2;
        var sphereMesh = sphereMesh2;
    }

    if (sphereMesh !== undefined) {
        scene.remove(sphereMesh);
        if (sphereMesh.geometry !== undefined) {
            sphereMesh.geometry.dispose();
            sphereMesh.geometry = undefined;
            if (sphereMesh.material.map)
                sphereMesh.material.map.dispose();
            if (sphereMesh.material.lightMap)
                sphereMesh.material.lightMap.dispose();
            if (sphereMesh.material.bumpMap)
                sphereMesh.material.bumpMap.dispose();
            if (sphereMesh.material.normalMap)
                sphereMesh.material.normalMap.dispose();
            if (sphereMesh.material.specularMap)
                sphereMesh.material.specularMap.dispose();
            if (sphereMesh.material.envMap)
                sphereMesh.material.envMap.dispose();
            sphereMesh.material.dispose();
            sphereMesh.material = undefined;
            sphereMesh = undefined;
        }
    }
    //Graph.scene().add(edgeMesh);
}

function distanceVector(v1, v2) {
    var dx = v1.x - v2.x;
    var dy = v1.y - v2.y;
    var dz = v1.z - v2.z;
    return Math.sqrt(dx * dx + dy * dy + dz * dz);
}

function resetBiplotBn() {
    var nms = Object.keys(biplotArrows);
    for (var i = 0; i < nms.length; i++) {
        var nm = nms[i];
        deleteMesh(biplotArrows[nm], Scatter.scene());
        delete biplotArrows[nm];
    }
    nms = Object.keys(biplotLabels);
    for (var i = 0; i < nms.length; i++) {
        var nm = nms[i];
        deleteMesh(biplotLabels[nm], Scatter.scene());
        delete biplotLabels[nm];
    }
}

function clearContour() {
    var nms = Object.keys(boundingClusters);
    for (var i = 0; i < nms.length; i++) {
        var nm = nms[i];
        deleteMetaSphere(nm, Scatter.scene());
        delete boundingClusters[nm];
    }
}

function deleteBiplot() {
    var nms = Object.keys(biplotArrows);
    for (var i = 0; i < nms.length; i++) {
        var nm = nms[i];
        deleteMesh(biplotArrows[nm], Scatter.scene());
        delete biplotArrows[nm];
    }
    nms = Object.keys(biplotLabels);
    for (var i = 0; i < nms.length; i++) {
        var nm = nms[i];
        deleteMesh(biplotLabels[nm], Scatter.scene());
        delete biplotLabels[nm];
    }
}

function colorLinkByNode() {
    Scatter.graphData().links.forEach(function (l) {
        colorLineObj2(l, l.color);
    });
}

function setShading() {
    var val = $("#shadeOpt").val();
    ballOpt = val;
    //$('#shadedlg').dialog('close')
    $('#loader').show();
    setTimeout(function () {
        var path = imagePath + ballOpt + ".png";

        var spritex = new TextureLoader().load(path);
        Scatter.graphData().nodes.forEach(function (n) {
            const object = n.__threeObj;
            // use a sphere as a drag handle
            var nmaterial = new SpriteMaterial({
                map: spritex,
                color: n.color,
                fog: true,
                alphaTest: 0.1,
                opacity: n.opacity
            });
            object.material = nmaterial;
        });

        $('#loader').hide();
    }, 120);
}

function switchToLoading() {
    $("#loader").show();
    clearContour();
    if (current_main_plot === "loading") {
        return;
    }
    current_main_plot = "loading";
    var nav = gData.navigation;
    var main = gData.nodes;
    gData.navigation = main;
    gData.nodes = nav;
    gData.edgesMain = gData.links;
    gData.links = [];
    initScatter2();
    for (var propNm in contours_info_obj) {
        var cont = contours_info_obj[propNm];
        if (cont.type === "contour") {
            //computeContours(scene2, cont.ids, cont.color)
        } else {
            //computeEllipsoid(scene2, cont.ids, cont.color, cont.type)
        }
    }

    Scatter.graphData(gData);
    deleteSpherical("inset");
    setTimeout(function () {
        Scatter.graphData().nodes.forEach(function (nd) {
            nd.highlight = 0;
            colorNodeObj(nd, nd.tcolor);
            opaNodeObj(nd, 1);
            deleteOutline(nd);
        });

    }, 10);

    setTimeout(function () {
        initNodeAppearances();
        for (var propertyName in myTexts) {
            if (propertyName.includes("tick@_main" + Scatter.scene().uuid)) {
                myTexts[propertyName].visible = false;
            } else if (propertyName.includes("tick@_loading" + Scatter.scene().uuid)) {
                myTexts[propertyName].visible = true;
            } else if (propertyName.includes("tick@_main" + scene2.uuid)) {
                myTexts[propertyName].visible = true;
            } else if (propertyName.includes("tick@_loading" + scene2.uuid)) {
                myTexts[propertyName].visible = false;
            }
        }

        for (var propertyName in gridsArr) {
            if (propertyName.includes("_loading_" + Scatter.scene().uuid)) {
                gridsArr[propertyName].material.visible = true;
            } else if (propertyName.includes("_main_" + scene2.uuid)) {
                gridsArr[propertyName].material.visible = true;
            } else {
                gridsArr[propertyName].material.visible = false;
            }
        }
        clearAllAxisTexts();

        generateAxisLabel(Scatter.scene());
        generateAxisLabel(scene2);
        $("#loader").hide();
    }, 1000);

}

function switchToScore() {
    $("#loader").show();
    clearContour();

    //$("#loadingOpt").attr("disabled", false);
    if (current_main_plot === "score") {
        return;
    }
    current_main_plot = "score";
    var nav = gData.navigation;
    var main = gData.nodes;
    gData.navigation = main;
    gData.nodes = nav;

    gData.edgesMain = null;
    initScatter2();
    //computeContours(Scatter.scene())
    for (var propNm in contours_info_obj) {
        var cont = contours_info_obj[propNm];
        if (cont.type === "contour") {
            //computeContours(Scatter.scene(), cont.ids, cont.color)
        } else {
            //computeEllipsoid(Scatter.scene(), cont.ids, cont.color, cont.type)
        }
    }
    deleteSpherical("main");

    Scatter.graphData(gData).linkOpacity(0);
    setTimeout(function () {
        Scatter.graphData().nodes.forEach(function (nd) {
            nd.highlight = 0;
            colorNodeObj(nd, nd.tcolor);
            if (nd.meta === "mcia.seg") {
                opaNodeObj(nd, 0);
            } else {
                opaNodeObj(nd, 1);
            }
            sizeNodeObj(nd, 1);
            deleteOutline(nd);
        });

    }, 10);
    setTimeout(function () {
        initNodeAppearances();
        for (var propertyName in myTexts) {
            if (propertyName.includes("tick@_main" + Scatter.scene().uuid)) {
                myTexts[propertyName].visible = true;
            } else if (propertyName.includes("tick@_loading" + Scatter.scene().uuid)) {
                myTexts[propertyName].visible = false;
            } else if (propertyName.includes("tick@_main" + scene2.uuid)) {
                myTexts[propertyName].visible = false;
            } else if (propertyName.includes("tick@_loading" + scene2.uuid)) {
                myTexts[propertyName].visible = true;
            }
        }

        for (var propertyName in gridsArr) {
            if (propertyName.includes("_main_" + Scatter.scene().uuid)) {
                gridsArr[propertyName].material.visible = true;
            } else if (propertyName.includes("_loading_" + scene2.uuid)) {
                gridsArr[propertyName].material.visible = true;
            } else {
                gridsArr[propertyName].material.visible = false;
            }
        }
        clearAllAxisTexts();

        generateAxisLabel(Scatter.scene());
        generateAxisLabel(scene2);

        $("#loader").hide();
    }, 1000);


}

function greyOutNodes() {

    if (current_main_plot === "score") {
        scene2.traverse(function (child) {
            if (child instanceof Mesh) {
                if (child.userData.nodeData !== undefined) {
                    var n = child.userData.nodeData;
                    var point = new Vector3(n.fx, n.fy, n.fz);
                    if (sphere2.containsPoint(point)) {
                        var colorValue = parseInt("0xd6d6d6", 16);
                        var col = new Color(colorValue);
                        child.material.color = col;
                        child.material.opacity = 0.2;
                    }
                }
            }
        });

    } else {

        var nodes = Scatter.graphData().nodes;
        nodes.forEach(function (n) {
            var point = new Vector3(n.fx, n.fy, n.fz);
            if (sphere1.containsPoint(point)) {

                colorNodeObj(n, "#d6d6d6");
                opaNodeObj(n, 0.2);
            }
        });

    }
}


function doEncasing(type) {
    var meta, encasingType, confVal, alphaVal;
    if (type === "biplot") {
        meta = $('#biplotMetaOpt').val();
        encasingType = $('#biplotEncasingType').val();
        confVal = $('#textInputBiplotConf').val();
        alphaVal = $('#textInputBiplotAlpha').val();
    } else {
        meta = $('#metaopt').val();
        encasingType = $('#scoreEncasingType').val();
        confVal = $('#textInputEncasingConf').val();
        var alphaVal = $('#textInputEncasingAlpha').val();
    }

    var colorValue = parseInt(arrowColorU.replace("#", "0x"), 16);
    var nodes_vec = [];
    var xArr = [];
    var yArr = [];
    var zArr = [];
    var meansArr = [];
    curr_meta = meta;
    if (gData.metaCol.type === "gradient") {          // ← continuous case
        var idsAll = [], xArr = [], yArr = [], zArr = [];

        Scatter.graphData().nodes.forEach(function (n) {
            idsAll.push(n.id);
            xArr.push(n.fx);
            yArr.push(n.fy);
            zArr.push(n.fz);
        });

        var meanXYZ = v(meanF(xArr), meanF(yArr), meanF(zArr));
        meansArr.push(meanXYZ);

        // use a neutral colour (e.g. grey) or any default
        computeEncasing(idsAll, encasingType, "#3c28d4", "All", confVal, alphaVal);
        deleteSpherical("main");
    } else {                                          // ← discrete case
        var arr = gData.meta[curr_meta];
        var set = [...new Set(arr)];
        setTimeout(function () {
            for (var i = 0; i < set.length; i++) {
                var idsArr = [], xArr = [], yArr = [], zArr = [], col = {};

                Scatter.graphData().nodes.forEach(function (n) {
                    if (n.meta === set[i]) {
                        idsArr.push(n.id);
                        xArr.push(n.fx);
                        yArr.push(n.fy);
                        zArr.push(n.fz);
                        col[set[i]] = n.color;
                    }
                });

                var meanXYZ = v(meanF(xArr), meanF(yArr), meanF(zArr));
                meansArr.push(meanXYZ);

                (function (i) {
                    computeEncasing(idsArr, encasingType, col[set[i]],
                            set[i], confVal, alphaVal);
                }(i));
            }
            deleteSpherical("main");
        }, 1);
    }

    /*
     for (var i = 0; i < set.length; i++) {
     dir = meansArr[i];
     console.log(set[i])
     var length = distanceVector(origin, dir)
     var arrow = new ArrowHelper(dir.normalize(), origin, length, colored, 0.1 * length, 0.4 * 0.1 * length);
     var text = createText2D(set[i], Scatter.scene().uuid);
     biplotArrows[set[i]] = arrow;
     biplotLabels[set[i]] = text;
     Scatter.scene().add(arrow)
     Scatter.scene().add(text)
     text.position.set(dir.x, dir.y, dir.z);
     
     }
     */
    //$.messager.progress('close');
    //$("#biplotdlg2").dialog("close");
}

function globalEncasing(callback) {
    if (current_main_plot !== "score") {
        switchToScoreCallBack(function () {
            doEncasing("encasing", false);
        });
    } else {

        console.log("encasing");
        doEncasing("encasing", false);
    }
    callback();
    //$("#encasingdlg").dialog("close");
}

function switchToLoadingCallBack(callBack) {
    //$("#loadingOpt").attr("disabled", true);
    current_main_plot = "loading";
    //deleteClusteringShapes(Scatter.scene())
    var nav = gData.navigation;
    var main = gData.nodes;
    gData.navigation = main;
    gData.nodes = nav;
    gData.edgesMain = gData.links;
    gData.links = [];
    initScatter2();
    for (var propNm in contours_info_obj) {
        var cont = contours_info_obj[propNm];
        if (cont.type === "contour") {
            //computeContours(scene2, cont.ids, cont.color)
        } else {
            //computeEllipsoid(scene2, cont.ids, cont.color, cont.type)
        }
    }
    Scatter.graphData(gData);
    deleteSpherical("inset");
    setTimeout(function () {
        Scatter.graphData().nodes.forEach(function (nd) {
            nd.highlight = 0;
            colorNodeObj(nd, nd.tcolor);
            if (nd.meta === "mcia.seg") {
                opaNodeObj(nd, 0);
            } else {
                opaNodeObj(nd, 1);
            }
            deleteOutline(nd);
        });
        addLoadingSphere("main", 500, sphereColor, sphereOpacity);

        var val = $("#loadingOpt").val();
        if (val !== "both") {
            var nodes = Scatter.graphData().nodes;
            nodes.forEach(function (n) {
                if (!n.omicstype.includes(val)) {
                    n.__threeObj.visible = false;
                } else {
                    n.__threeObj.visible = true;

                }
            });
        }
        return callBack();
    }, 10);
}


function switchToScoreCallBack(callBack) {
    //$("#loadingOpt").attr("disabled", false);
    current_main_plot = "score";
    //deleteClusteringShapes(scene2)
    var nav = gData.navigation;
    var main = gData.nodes;
    gData.navigation = main;
    gData.nodes = nav;
    if (gData.edgesMain !== undefined) {
        gData.links = gData.edgesMain;
    } else {
        gData.links = gData.edges;
    }
    gData.edgesMain = null;
    initScatter2();
    for (var propNm in contours_info_obj) {
        var cont = contours_info_obj[propNm];
        if (cont.type === "contour") {
            //computeContours(Scatter.scene(), cont.ids, cont.color)
        } else {
            //computeEllipsoid(Scatter.scene(), cont.ids, cont.color, cont.type)
        }
    }
    deleteSpherical("main");
    Scatter.graphData(gData);
    setTimeout(function () {
        Scatter.graphData().nodes.forEach(function (nd) {
            nd.highlight = 0;
            colorNodeObj(nd, nd.tcolor);
            opaNodeObj(nd, 1);
            sizeNodeObj(nd, 1);
            deleteOutline(nd);
        });

        var val = $("#loadingOpt").val();
        if (val !== "both") {
            scene2.traverse(function (child) {
                if (child instanceof Mesh) {
                    var n = child.userData.nodeData;
                    if (n !== undefined) {

                        if (!n.omicstype.includes(val)) {
                            child.visible = false;
                        } else {
                            child.visible = true;

                        }
                    }
                }
            });
        }
        return callBack();
    }, 10);



}

function biplotScore() {
    if (current_main_plot === "score") {
        doBiplotScore();

    } else {
        switchToScoreCallBack(function () {
            doBiplotScore();
        });
    }
}

function doBiplotScore() {

    var opt = $('#viewSelOpt').val();
    var selIds = [];

    if (opt === "auto") {
        var val = parseFloat($('#biplotSliderScore').val());
        for (var propNm in gData.sigMat) {
            for (var i = 0; i < gData.sigMat[propNm].length; i++) {
                if (i < val) {
                    selIds.push(gData.sigMat[propNm][i].ids);
                }
            }
        }
    } else {
        var selIds = $('#batchBiplot').val().split('\n');

    }

    var nodes = Scatter.graphData().nodes;
    var length;
    var dir = new Vector3(1, 0, 0);
    var origin = new Vector3(0, 0, 0);
    var colorValue = parseInt(arrowColorU.replace("#", "0x"), 16);
    var colored = new Color(colorValue);
    var count = 0;
    var textBool = $('#biplotTextDisplayOpt').val();
    scene2.traverse(function (child) {
        if (child instanceof Mesh) {
            if (child.userData.nodeData !== undefined) {
                if (selIds.indexOf(child.userData.nodeData.id) !== -1 || selIds.indexOf(child.userData.nodeData.label) !== -1) {
                    var n = child.userData.nodeData;
                    dir = new Vector3(n.fx, n.fy, n.fz);
                    var length = distanceVector(origin, dir);
                    var arrow = new ArrowHelper(dir.normalize(), origin, length, colored, 0.1 * length, 0.4 * 0.1 * length);
                    if (biplotArrows[n.id] !== undefined) {
                        deleteMesh(biplotLabels[n.id], Scatter.scene());
                        delete biplotLabels[n.id];
                        deleteMesh(biplotArrows[n.id], Scatter.scene());
                        delete biplotArrows[n.id];
                    }
                    biplotArrows[n.id] = arrow;
                    Scatter.scene().add(arrow);

                    if (textBool !== "none") {
                        var text = createText2D(n.label, "", Scatter.scene().uuid, "biplot");
                        biplotLabels[n.id] = text;
                        Scatter.scene().add(text);
                        text.position.set(n.fx, n.fy, n.fz);

                    }

                    count++;
                }
            }
        }
    });
    if (count === 0) {
        $.messager.alert('Error', "No node has been matched in the loading plot!", 'error');
    }

}



function setGradientColor(col1, col2) {
    var canvas = document.createElement("canvas");
    canvas.width = window.innerWidth;
    canvas.height = window.innerHeight;
    var ctx = canvas.getContext("2d");
    var grd = ctx.createLinearGradient(0, 0, 0, window.innerHeight);
    grd.addColorStop(0, col1);
    grd.addColorStop(1, col2);

    ctx.fillStyle = grd;
    ctx.fillRect(0, 0, window.innerWidth, window.innerHeight);

    var texture = new CanvasTexture(canvas);
    //console.log(col1)
    // main part
    if (mainCheckBool) {
        Scatter.scene().background = texture;
    } else {
        scene2.background = texture;
    }

}

function updateBiplotText() {
    var textBool = $('#biplotTextDisplayOpt').val();
    if (textBool === "none") {
        var nms = Object.keys(biplotLabels);
        for (var i = 0; i < nms.length; i++) {
            var nm = nms[i];
            biplotLabels[nm].visble = false;
        }
    } else {
        var nms = Object.keys(biplotLabels);
        for (var i = 0; i < nms.length; i++) {
            var nm = nms[i];
            biplotLabels[nm].visble = true;
            biplotLabels[nm].color = biplotTextColorU;
        }
    }
}

function addArrows(selIds) {
    var nms = Object.keys(biplotArrows);
    if (selIds.length === 1) {
        if (nms.indexOf(selIds[0]) !== -1) {
            return;
        }
    } else {

    }
    var textBool = $('#biplotTextDisplayOpt').val();
    var dir = new Vector3(1, 0, 0);
    var origin = new Vector3(0, 0, 0);
    var colorValue = parseInt(highlightColor.replace("#", "0x"), 16);
    var colored = new Color(colorValue);
    var count = 0;
    scene2.traverse(function (child) {
        if (child instanceof Mesh) {
            if (child.userData.nodeData !== undefined) {
                if (selIds.indexOf(child.userData.nodeData.id) !== -1 || selIds.indexOf(child.userData.nodeData.label) !== -1) {

                    var n = child.userData.nodeData;
                    dir = new Vector3(n.fx, n.fy, n.fz);
                    var length = distanceVector(origin, dir);
                    var arrow = new ArrowHelper(dir.normalize(), origin, length, colored, 0.1 * length, 0.4 * 0.1 * length);
                    if (biplotArrows[n.id] !== undefined) {
                        deleteMesh(biplotLabels[n.id], Scatter.scene());
                        delete biplotLabels[n.id];
                        deleteMesh(biplotArrows[n.id], Scatter.scene());
                        delete biplotArrows[n.id];
                    }
                    biplotArrows[n.id] = arrow;
                    Scatter.scene().add(arrow);

                    if (textBool !== "none") {
                        var text = createText2D(n.label, "", Scatter.scene().uuid, "biplot");
                        biplotLabels[n.id] = text;
                        Scatter.scene().add(text);
                        text.position.set(n.fx, n.fy, n.fz);

                    }

                    count++;
                }
            }
        }
    });
}

function deleteArrows(selIds) {
    if (current_main_plot === "score") {
        var nms = Object.keys(biplotArrows);
        for (var i = 0; i < nms.length; i++) {
            var nm = nms[i];
            if (selIds.indexOf(nm) !== -1) {
                deleteMesh(biplotArrows[nm], Scatter.scene());
                delete biplotArrows[nm];
            }
        }
        nms = Object.keys(biplotLabels);
        for (var i = 0; i < nms.length; i++) {
            var nm = nms[i];
            if (selIds.indexOf(nm) !== -1) {
                deleteMesh(biplotLabels[nm], Scatter.scene());
                delete biplotLabels[nm];
            }
        }
    }
}

function setDualView() {
    embeddedCollapsed = false;
    $('#loader').show();
    //toggleOmicsType();
    $('#loader').hide();
}


function doBiplotLoading(type) {
    if (type === "biplot") {
        var meta = $('#biplotMetaOpt').val();
        var encasingType = $('#biplotEncasingType').val();
        var confVal = $('#textInputBiplotConf').val();
        var alphaVal = $('#textInputBiplotAlpha').val();
    } else {
        var meta = $('#encasingMetaOpt').val();
        var encasingType = $('#scoreEncasingType').val();
        var confVal = $('#textInputEncasingConf').val();
        var alphaVal = $('#textInputEncasingAlpha').val();
    }

    //var alphaVal = 0.7
    var selIds = [];
    var length;
    var dir = new Vector3(1, 0, 0);
    var origin = new Vector3(0, 0, 0);
    var colorValue = parseInt(arrowColorU.replace("#", "0x"), 16);
    var colored = new Color(colorValue);


    var nodes_vec = [];
    var xArr = [];
    var yArr = [];
    var zArr = [];
    var meansArr = [];
    curr_meta = meta;
    loadMeta();
    var arr = gData.meta[curr_meta];
    var set = [...new Set(arr)];
    setTimeout(function () {
        for (var i = 0; i < set.length; i++) {
            var idsArr = [];
            var col = {};
            scene2.traverse(function (child) {
                if (child instanceof Mesh) {
                    var n = child.userData.nodeData;
                    if (n !== undefined) {
                        if (n.metatype === set[i]) {
                            nodes_vec.push(n.id);
                            xArr.push(n.fx);
                            yArr.push(n.fy);
                            zArr.push(n.fz);
                            idsArr.push(n.id);
                            col[set[i]] = n.color;
                        }
                    }
                }
            });

            var meanX = meanF(xArr);
            var meanY = meanF(yArr);
            var meanZ = meanF(zArr);
            var meanXYZ = v(meanX, meanY, meanZ);
            meansArr.push(meanXYZ);

            (function (i) {
                computeEncasing(idsArr, encasingType, col[set[i]], set[i], confVal, alphaVal, false, set[i]);
                //rest of the code
            }(i));

        }
        deleteSpherical("main");
    }, 1);

    // $("#biplotdlg2").dialog("close");
}

function switchToScoreFromDialog() {

    $("#loader").show();
    globalEncasing(function () {
        $("#loader").hide()
    });

}

function handleRadio(myRadio) {
    var val = myRadio.value;
    if (val === "no") {
        document.getElementById("myTableFieldSet").disabled = true;
    } else {
        document.getElementById("myTableFieldSet").disabled = false;
    }

}

function removeAllOverlays() {
    var nms = Object.keys(boundingClusters);
    for (var i = 0; i < nms.length; i++) {
        var nm = nms[i];
        deleteMetaSphere(nm, Scatter.scene());
    }
    nms = Object.keys(biplotArrows);
    for (var i = 0; i < nms.length; i++) {
        var nm = nms[i];
        deleteMesh(biplotArrows[nm], Scatter.scene());
        delete biplotArrows[nm];
    }
    nms = Object.keys(biplotLabels);
    for (var i = 0; i < nms.length; i++) {
        var nm = nms[i];
        deleteMesh(biplotLabels[nm], Scatter.scene());
        delete biplotLabels[nm];
    }
}

function minMaxCoords(points) {
    // Initialize min and max values for each axis
    let minX = Infinity, minY = Infinity, minZ = Infinity;
    let maxX = -Infinity, maxY = -Infinity, maxZ = -Infinity;
    // Iterate over each point in the array
    points.forEach(function (point) {
        // Update the min and max values for each axis
        minX = Math.min(minX, point.origX);
        minY = Math.min(minY, point.origY);
        minZ = Math.min(minZ, point.origZ);
        maxX = Math.max(maxX, point.origX);
        maxY = Math.max(maxY, point.origY);
        maxZ = Math.max(maxZ, point.origZ);

    });

    // Return an object with the min and max values for each axis
    return {
        minX: minX,
        minY: minY,
        minZ: minZ,
        maxX: maxX,
        maxY: maxY,
        maxZ: maxZ,
    };
}

function ToQuads(g) {
    let p = g.parameters;
    let segmentsX = (g.type == "TorusBufferGeometry" ? p.tubularSegments : p.radialSegments) || p.widthSegments || p.thetaSegments || (p.points.length - 1) || 1;
    let segmentsY = (g.type == "TorusBufferGeometry" ? p.radialSegments : p.tubularSegments) || p.heightSegments || p.phiSegments || p.segments || 1;
    let indices = [];
    for (let i = 0; i < segmentsY + 1; i++) {
        let index11 = 0;
        let index12 = 0;
        for (let j = 0; j < segmentsX; j++) {
            index11 = (segmentsX + 1) * i + j;
            index12 = index11 + 1;
            let index21 = index11;
            let index22 = index11 + (segmentsX + 1);
            indices.push(index11, index12);
            if (index22 < ((segmentsX + 1) * (segmentsY + 1) - 1)) {
                indices.push(index21, index22);
            }
        }
        if ((index12 + segmentsX + 1) <= ((segmentsX + 1) * (segmentsY + 1) - 1)) {
            indices.push(index12, index12 + segmentsX + 1);
        }
    }
    g.setIndex(indices);
}

//jquery dialog centering
(function ($) {
    $.extend($.fn.window.methods, {
        moveTo: function (jq, param) {
            return jq.each(function () {
                var win = $(this).window('window');

                var canvas = document.getElementsByTagName("canvas")[0];
                var width = win.outerWidth();
                var height = win.outerHeight();
                var left = undefined;
                var top = undefined;

                $(this).window('move', {
                    left: 450,
                    top: 100
                });
            });
        }
    });
    $.extend($.fn.dialog.methods, {
        openCenter: function (jq, param) {
            return jq.each(function () {
                $(this).dialog('open');
                $(this).window('move', {
                    left: 450,
                    top: 100
                });
            });
        }
    });
})(jQuery);


function getBoxPlot(id) {
    console.log(id);

    $.ajax({
        beforeSend: function () {

        },
        dataType: "html",
        type: "GET",
        url: '/MetaboAnalyst/faces/AjaxCall',
        data: {function: 'loadingBoxPlot', id: id},
        async: false,
        success: function (result) {
            parent.PF('statusDialog').hide();
            parent.PF('FeatureView').show();
        }
    });
}


function getXICPlot(id) {

    $.ajax({
        beforeSend: function () {
            parent.PF('statusDialog').show();
        },
        dataType: "html",
        type: "POST",
        url: '/MetaboAnalyst/faces/AjaxCall',
        data: {function: 'loadingXICPlot', id: id},
        async: false,
        success: function (result) {
            parent.PF('statusDialog').hide();
            //PF('EIC').show();
            setTimeout(function () {
                //console.log(parent.updateXICs())
                //parent.updateXICs();
                parent.initSvgInteractions(result);
                parent.PF('BoxPlotdialog').show();

            }, 100);

        }
    });
}


function getTimeBoxPlot(id) {
    $.ajax({
        beforeSend: function () {

        },
        dataType: "html",
        type: "POST",
        url: '/MetaboAnalyst/faces/AjaxCall',
        data: {function: 'iPCALoadingBoxPlot', id: id},
        async: false,
        success: function (result) {
            parent.PF('FeatureView').show();
        }
    });
}

function updateLoading(opt, nb) {
    performLoad(nb, function (result) {
        if (result !== "NA") {
            updateLoad(opt, result);
        }
    });
}

function performLoad(nb, callBack) {
    $.ajax({
        beforeSend: function () {
        },
        dataType: "html",
        type: "POST",
        url: '/MetaboAnalyst/faces/AjaxCall',
        data: "function=updateLoading" + "&number=" + nb,
        async: true,
        cache: false,
        success: function (result) {
            return callBack(result);
        },
        error: function () {
        }
    });
}

function updateLoad(opt, result) {
    if (current_main_plot === "loading") {
        switchToScore();
    }
    var peaksJson = result.split("||")[0];
    var allJson = result.split("||")[1];
    if (opt !== "loading") {
        $.getJSON('/MetaboAnalyst' + allJson,
                function (raw_data) {
                    var i = 0;
                    Scatter.graphData().nodes.forEach(function (nd) {
                        var node = raw_data.nodes[i];
                        nd.fx = node.fx;
                        nd.fy = node.fy;
                        nd.fz = node.fz;
                        nd.x = node.fx;
                        nd.y = node.fy;
                        nd.z = node.fz;
                        i = i + 1;
                    });
                    Scatter.refresh();
                });
    }

    $.getJSON('/MetaboAnalyst' + peaksJson,
            function (raw_data) {

                if (opt === "loading") {
                    var i = 0;
                    Scatter.graphData().nodes.forEach(function (nd) {
                        var node = raw_data.nodes[i];
                        nd.fx = node.fx;
                        nd.fy = node.fy;
                        nd.fz = node.fz;
                        nd.x = node.fx;
                        nd.y = node.fy;
                        nd.z = node.fz;
                        i = i + 1;
                    });
                    Scatter.refresh();
                }

                gData.loading = raw_data.loading
                gData.navigation = gData.loading;
                var arr = [gData.navigation];
                for (var j = 0; j < arr.length; j++) {
                    for (var i = 0; i < arr[j].length; i++) {
                        var n = arr[j][i];
                        n.tcolor = n.colorb;
                        n.color = n.colorb;
                        if (arr[j] !== gData.navigation) {
                            n.tsize = (1 / Math.cbrt(arr[j].length)) * 120;
                            n.size = (1 / Math.cbrt(arr[j].length)) * 120;
                        } else {
                            if (gData.nodes.length > 500) {
                                n.tsize = n.size / 2;
                            } else {
                                n.tsize = n.size;
                            }
                        }
                        n.expcolor = n.expcolb;
                        n.opacity = 1;

                        delete n.x;
                        delete n.y;
                        delete n.z;
                        delete n.vx;
                        delete n.vy;
                        delete n.vz;
                    }
                }
                initScatter2();
            });
}


function performSpectraPCA(nb, callBack) {
    $.ajax({
        beforeSend: function () {

        },
        dataType: "html",
        type: "POST",
        url: '/MetaboAnalyst/faces/AjaxCall',
        data: "function=updateSpectraPCA" + "&number=" + nb,
        async: true,
        cache: false,
        success: function (result) {
            return callBack(result);
        },
        error: function () {
            $.messager.alert('Error', 'Failed to process the request!', 'error');
            $.messager.progress('close');
        }
    });
}

function getaTICPlot(fileNM) {

    $.ajax({
        beforeSend: function () {

        },
        dataType: "html",
        type: "POST",
        url: '/MetaboAnalyst/faces/AjaxCall',
        data: {function: 'loadingTICPlot', fileNM: fileNM},
        async: false,
        success: function (result) {
            parent.PF('statusDialog').hide();
            parent.PF('TIC').show();
        }
    });
}

//firefox issues
function setDialogSize() {

    $('#nodestyledlg').css("height", "auto")
    $('#nodestyledlg').css("width", "auto")
    $('#ttNode').css("width", "390px")
    $('#ttNode').css("height", "230px")
    //elements[0].removeProperty("width");
    $('.panel-title .panel-with-icon').css("width", "400px")
    //$('#nodestyledlg').dialog("open")
    //        $('#nodestyledlg').dialog("close")
    const elements = document.getElementsByClassName('window-header');
    for (let i = 0; i < elements.length; i++) {
        elements[i].style.removeProperty("width");
    }

}


function saveState() {

    savedState.scatterNodes = Scatter.graphData().nodes;
    savedState.cameraPosition = Scatter.cameraPosition();
    savedState.gradCol1 = gradCol1;
    savedState.gradCol2 = gradCol2;
    savedState.current_main_plot = current_main_plot;

    //gData.textDisplayOpt = val;

}

function sendToServer(callback) {
    var img = new Image();
    img.src = getImageDataURL();
    // You can uncomment the following lines if needed
    //container.src = img.src;
    //container.appendChild(img);

    setTimeout(function () {
        callback(img.src);
    }, 100);
}


function sendImageToServer(dataURL, type, callback) {
    sendImageToServerFull(dataURL, type, "png", callback);
}


function sendImageToServerFull(dataURL, type, format, callback = null) {
    $.ajax({
        beforeSend: function () {

        },
        dataType: "html",
        type: "POST",
        url: '/MetaboAnalyst/faces/AjaxCall',
        data: {function: 'decodeImageData', data: dataURL, name: type, format: format},
        async: false,
        cache: false,
        error: function () {
            console.log("Error during image saving");
            parent.PF("statusDialog").hide();

        },
        success: function () {
            console.log("Success: Image saving");
            if (typeof callback === "function") {
                callback();
            }
        }
    });
}

function sendJsonToServer(jsonData, type, generateReport) {
    $.ajax({
        beforeSend: function () {
            parent.PF("statusDialog").show();
        },
        dataType: "html",
        type: "POST",
        url: '/MetaboAnalyst/faces/AjaxCall',
        data: {function: 'receiveJsonData', data: jsonData, name: type},
        async: false,
        cache: false,
        error: function () {
            console.log("Error during JSON saving");

        },
        success: function () {
            console.log("Success: JSON saving");
            if (generateReport) {
                parent.PF("statusDialog").show();
                parent.generateReportFromJS();
            } else {
                parent.PF("statusDialog").hide();
            }
        }
    });
}

function getImageDataURL() {
    var img = new Image();
    var scene = Scatter.scene();
    var camera = Scatter.camera();

    if (gData.navigation !== "NA") {
        takeImageBool = true;
    }

    updateLabelPositions();
    Scatter.renderer().setViewport(0, 0, parseInt(Scatter.renderer().domElement.width), parseInt(Scatter.renderer().domElement.height));
    Scatter.renderer().setScissor(0, 0, parseInt(Scatter.renderer().domElement.width), parseInt(Scatter.renderer().domElement.height));
    Scatter.renderer().setPixelRatio(window.devicePixelRatio);
    Scatter.renderer().render(scene, camera);
    if (!embeddedCollapsed) {
        Scatter.renderer().setViewport(0, 0, 300 * camera.aspect - 50, 300);
        Scatter.renderer().setScissor(0, 0, 300 * camera.aspect - 50, 300);
        Scatter.renderer().render(scene2, camera);
    }
    return Scatter.renderer().domElement.toDataURL();

}

function initialSaveState(jsonNm) {
    console.log('initialSaveState + jsonNm' + "----> " + jsonNm);

    setTimeout(function () {
        //check if there is saved state
        var fileNm = "";
        var type = "";
        if (parent.window.location.href.includes("LivePCAView")) {
            type = "ipca_";
        } else if (parent.window.location.href.includes("PCAView")) {
            type = "pca_";
        } else if (parent.window.location.href.includes("SparsePLSDAView")) {
            type = "spls_";
        } else if (parent.window.location.href.includes("PLSDAView")) {
            type = "pls_";
        } else if (parent.window.location.href.includes("SpectraResult")) {
            type = "unknown";
        }
        if (jsonNm.includes(type)) {
            fileNm = jsonNm.replace(type, "report_" + type);
        } else {
            fileNm = jsonNm;
        }
        console.log('/MetaboAnalyst' + "/" + fileNm);
        console.log('/MetaboAnalyst+fileNm' + "--->" + fileNm);

        fetch('/MetaboAnalyst' + "/" + fileNm)
                .then(response => {
                    if (response.ok) {
                        console.log('File exists. Reload previous state');
                        $.getJSON('/MetaboAnalyst' + "/" + fileNm, function (data) {
                            savedState = data;
                            Scatter.cameraPosition(savedState.cameraPosition);
                            if ("gradCol1" in savedState) {
                                changeBackground(savedState.gradCol1, savedState.gradCol2);
                            }
                            current_main_plot = savedState.current_main_plot;
                            if (savedState.current_main_plot === "score") {
                                Scatter.graphData({nodes: savedState.scatterNodes, links: []}).cooldownTicks(1).refresh();
                            } else {
                                switchToLoading();
                            }
                            setTimeout(function () {

                                Scatter.graphData().nodes.forEach(function (nd) {
                                    nd.highlight = 0;
                                    colorNodeObj(nd, nd.tcolor);
                                    if (nd.meta === "mcia.seg") {
                                        opaNodeObj(nd, 0);
                                    } else {
                                        opaNodeObj(nd, 1);
                                    }
                                    materialNodeObj(nd, "MeshPhysicalMaterial");
                                    if (nd.shape !== undefined) {
                                        shapeNodeObj(nd, nd.shape);
                                    }
                                    deleteOutline(nd);
                                });
                                Scatter.renderer().setAnimationLoop(updateLabelPositions);
                                if ("encasingFileNames" in savedState) {
                                    if (current_main_plot === "score") {
                                        doEncasing("encasing", true);
                                    }
                                }
                            }, 100);
                        });
                        parent.PF("statusDialog").hide();//OK

                    } else {
                        Scatter.graphData().nodes.forEach(function (nd) {
                            nd.highlight = 0;
                            colorNodeObj(nd, nd.tcolor);
                            if (nd.meta === "mcia.seg") {
                                opaNodeObj(nd, 0);
                            } else {
                                opaNodeObj(nd, 1);
                            }
                            materialNodeObj(nd, "MeshPhysicalMaterial");
                            if (nd.shape !== undefined) {
                                shapeNodeObj(nd, nd.shape);
                            }
                            deleteOutline(nd);
                        });
                        Scatter.renderer().setAnimationLoop(updateLabelPositions);
                        setTimeout(function () {
                            handleSaveEvent(false);
                            parent.PF("statusDialog").hide();//OK

                        }, 2000);
                    }
                })
                .catch(error => {
                    console.error('There was a problem with the fetch operation:', error.message);
                });

        //Scatter.backgroundColor("#fff");
        //$('#p').panel('close')
    }, 200);

}

window.myModuleFunctions = {
    initialSaveState: initialSaveState
};


function handleSaveEvent(reportFlag) {
    sendToServer(function (result) {
        var type = "";
        if (parent.window.location.href.includes("LivePCAView")) {
            type = "ipca_3d";
        } else if (parent.window.location.href.includes("PCAView")) {
            type = "pca_3d";
        } else if (parent.window.location.href.includes("SparsePLSDAView")) {
            type = "splsda_3d";
        } else if (parent.window.location.href.includes("PLSDAView")) {
            type = "plsda_3d";
        } else if (parent.window.location.href.includes("SpectraResult")) {
            type = "scores_3d";
        }
        //console.log(type);
        captureImageWithLegend(result, type);
        //sendImageToServer(dataURL, type);
        saveState();
        sendJsonToServer(JSON.stringify(savedState), type, reportFlag);
    });
}


function initReportFunctions() {
    $('#reportBn').bind('click keypress', function (event) {
        handleSaveEvent(false);
    });

    var element = $(parent.window.document).find("#sidebar-form\\:m_report");
    element.unbind('click keypress').bind('click keypress', function (event) {
        handleSaveEvent(true);
    });
}

function captureImageWithLegend(dataURL, type) {
    // Create an off-screen canvas
    const canvas = document.createElement('canvas');
    const ctx = canvas.getContext('2d');

// Load the rendered scene image
    const sceneImage = new Image();
    sceneImage.src = dataURL;
    sceneImage.onload = () => {

// Create an off-screen canvas
        const canvas = document.createElement('canvas');
        canvas.width = sceneImage.width;
        canvas.height = sceneImage.height;
        const ctx = canvas.getContext('2d');

        // Draw the scene image onto the canvas
        ctx.drawImage(sceneImage, 0, 0);
        // Capture the HTML legend with html2canvas
        const legendElement = document.getElementById('myLegend');
        legendUtils.updateTextColor(axisColorU);
        html2canvas(legendElement, {logging: true, useCORS: true, backgroundColor: null}).then(legendCanvas => {
            // Draw the legend canvas onto your scene canvas
            ctx.drawImage(legendCanvas, canvas.width - legendCanvas.width, 0); // Adjust position as needed

            // Export the combined canvas to an image
            const finalDataURL = canvas.toDataURL('image/png');
            sendImageToServer(finalDataURL, type);
            // You can now use this finalDataURL as the source for an <img> tag or download it
        });

    };

}
