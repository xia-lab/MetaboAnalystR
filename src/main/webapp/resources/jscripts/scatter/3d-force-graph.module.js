import { AmbientLight, DirectionalLight, Vector3 } from "../three.module.js";
import { DragControls } from './DragControls.js';
import ThreeForceGraph from './three-forcegraph.module.js';
import ThreeRenderObjects from './three-render-objects.module.js';

import accessorFn from './accessor-fn.module.js';
import Kapsule from './kapsule.module.js';
//import {boundingModulesArr} from './scatter.module.js';

function styleInject(css, ref) {
  if (ref === void 0) ref = {};
  var insertAt = ref.insertAt;

  if (!css || typeof document === 'undefined') {
    return;
  }

  var head = document.head || document.getElementsByTagName('head')[0];
  var style = document.createElement('style');
  style.type = 'text/css';

  if (insertAt === 'top') {
    if (head.firstChild) {
      head.insertBefore(style, head.firstChild);
    } else {
      head.appendChild(style);
    }
  } else {
    head.appendChild(style);
  }

  if (style.styleSheet) {
    style.styleSheet.cssText = css;
  } else {
    style.appendChild(document.createTextNode(css));
  }
}

var css_248z = ".graph-info-msg {\n  top: 50%;\n  width: 100%;\n  text-align: center;\n  color: lavender;\n  opacity: 0.7;\n  font-size: 22px;\n  position: absolute;\n  font-family: Sans-serif;\n}\n\n.scene-container .clickable {\n  cursor: pointer;\n}\n\n.scene-container .grabbable {\n  cursor: move;\n  cursor: grab;\n  cursor: -moz-grab;\n  cursor: -webkit-grab;\n}\n\n.scene-container .grabbable:active {\n  cursor: grabbing;\n  cursor: -moz-grabbing;\n  cursor: -webkit-grabbing;\n}";
styleInject(css_248z);

function ownKeys(object, enumerableOnly) {
  var keys = Object.keys(object);

  if (Object.getOwnPropertySymbols) {
    var symbols = Object.getOwnPropertySymbols(object);
    enumerableOnly && (symbols = symbols.filter(function (sym) {
      return Object.getOwnPropertyDescriptor(object, sym).enumerable;
    })), keys.push.apply(keys, symbols);
  }

  return keys;
}

function _objectSpread2(target) {
  for (var i = 1; i < arguments.length; i++) {
    var source = null != arguments[i] ? arguments[i] : {};
    i % 2 ? ownKeys(Object(source), !0).forEach(function (key) {
      _defineProperty(target, key, source[key]);
    }) : Object.getOwnPropertyDescriptors ? Object.defineProperties(target, Object.getOwnPropertyDescriptors(source)) : ownKeys(Object(source)).forEach(function (key) {
      Object.defineProperty(target, key, Object.getOwnPropertyDescriptor(source, key));
    });
  }

  return target;
}

function _defineProperty(obj, key, value) {
  if (key in obj) {
    Object.defineProperty(obj, key, {
      value: value,
      enumerable: true,
      configurable: true,
      writable: true
    });
  } else {
    obj[key] = value;
  }

  return obj;
}

function _toConsumableArray(arr) {
  return _arrayWithoutHoles(arr) || _iterableToArray(arr) || _unsupportedIterableToArray(arr) || _nonIterableSpread();
}

function _arrayWithoutHoles(arr) {
  if (Array.isArray(arr)) return _arrayLikeToArray(arr);
}

function _iterableToArray(iter) {
  if (typeof Symbol !== "undefined" && iter[Symbol.iterator] != null || iter["@@iterator"] != null) return Array.from(iter);
}

function _unsupportedIterableToArray(o, minLen) {
  if (!o) return;
  if (typeof o === "string") return _arrayLikeToArray(o, minLen);
  var n = Object.prototype.toString.call(o).slice(8, -1);
  if (n === "Object" && o.constructor) n = o.constructor.name;
  if (n === "Map" || n === "Set") return Array.from(o);
  if (n === "Arguments" || /^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(n)) return _arrayLikeToArray(o, minLen);
}

function _arrayLikeToArray(arr, len) {
  if (len == null || len > arr.length) len = arr.length;

  for (var i = 0, arr2 = new Array(len); i < len; i++) arr2[i] = arr[i];

  return arr2;
}

function _nonIterableSpread() {
  throw new TypeError("Invalid attempt to spread non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.");
}

function linkKapsule (kapsulePropName, kapsuleType) {
  var dummyK = new kapsuleType(); // To extract defaults

  return {
    linkProp: function linkProp(prop) {
      // link property config
      return {
        "default": dummyK[prop](),
        onChange: function onChange(v, state) {
          state[kapsulePropName][prop](v);
        },
        triggerUpdate: false
      };
    },
    linkMethod: function linkMethod(method) {
      // link method pass-through
      return function (state) {
        var kapsuleInstance = state[kapsulePropName];

        for (var _len = arguments.length, args = new Array(_len > 1 ? _len - 1 : 0), _key = 1; _key < _len; _key++) {
          args[_key - 1] = arguments[_key];
        }

        var returnVal = kapsuleInstance[method].apply(kapsuleInstance, args);
        return returnVal === kapsuleInstance ? this // chain based on the parent object, not the inner kapsule
        : returnVal;
      };
    }
  };
}

var three = window.THREE ? window.THREE // Prefer consumption from global THREE, if exists
: {
  AmbientLight: AmbientLight,
  DirectionalLight: DirectionalLight,
  Vector3: Vector3
};

var CAMERA_DISTANCE2NODES_FACTOR = 170; //
// Expose config from forceGraph

var bindFG = linkKapsule('forceGraph', ThreeForceGraph);
var linkedFGProps = Object.assign.apply(Object, _toConsumableArray(['jsonUrl', 'graphData', 'numDimensions', 'dagMode', 'dagLevelDistance', 'dagNodeFilter', 'onDagError', 'nodeRelSize', 'nodeId', 'nodeVal', 'nodeResolution', 'nodeColor', 'nodeAutoColorBy', 'nodeOpacity', 'nodeVisibility', 'nodeThreeObject', 'nodeThreeObjectExtend', 'linkSource', 'linkTarget', 'linkVisibility', 'linkColor', 'linkAutoColorBy', 'linkOpacity', 'linkWidth', 'linkResolution', 'linkCurvature', 'linkCurveRotation', 'linkMaterial', 'linkThreeObject', 'linkThreeObjectExtend', 'linkPositionUpdate', 'linkDirectionalArrowLength', 'linkDirectionalArrowColor', 'linkDirectionalArrowRelPos', 'linkDirectionalArrowResolution', 'linkDirectionalParticles', 'linkDirectionalParticleSpeed', 'linkDirectionalParticleWidth', 'linkDirectionalParticleColor', 'linkDirectionalParticleResolution', 'forceEngine', 'd3AlphaDecay', 'd3VelocityDecay', 'd3AlphaMin', 'ngraphPhysics', 'warmupTicks', 'cooldownTicks', 'cooldownTime', 'onEngineTick', 'onEngineStop'].map(function (p) {
  return _defineProperty({}, p, bindFG.linkProp(p));
})));
var linkedFGMethods = Object.assign.apply(Object, _toConsumableArray(['refresh', 'getGraphBbox', 'd3Force', 'd3ReheatSimulation', 'emitParticle'].map(function (p) {
  return _defineProperty({}, p, bindFG.linkMethod(p));
}))); // Expose config from renderObjs

var bindRenderObjs = linkKapsule('renderObjs', ThreeRenderObjects);
var linkedRenderObjsProps = Object.assign.apply(Object, _toConsumableArray(['width', 'height', 'backgroundColor', 'showNavInfo', 'enablePointerInteraction'].map(function (p) {
  return _defineProperty({}, p, bindRenderObjs.linkProp(p));
})));
var linkedRenderObjsMethods = Object.assign.apply(Object, _toConsumableArray(['cameraPosition', 'postProcessingComposer'].map(function (p) {
  return _defineProperty({}, p, bindRenderObjs.linkMethod(p));
})).concat([{
  graph2ScreenCoords: bindRenderObjs.linkMethod('getScreenCoords'),
  screen2GraphCoords: bindRenderObjs.linkMethod('getSceneCoords')
}])); //

var _3dForceGraph = Kapsule({
  props: _objectSpread2(_objectSpread2({
    nodeLabel: {
      "default": 'name',
      triggerUpdate: false
    },
    linkLabel: {
      "default": 'name',
      triggerUpdate: false
    },
    linkHoverPrecision: {
      "default": 1,
      onChange: function onChange(p, state) {
        return state.renderObjs.lineHoverPrecision(p);
      },
      triggerUpdate: false
    },
    enableNavigationControls: {
      "default": true,
      onChange: function onChange(enable, state) {
        var controls = state.renderObjs.controls();

        if (controls) {
          controls.enabled = enable;
        }
      },
      triggerUpdate: false
    },
    enableNodeDrag: {
      "default": true,
      triggerUpdate: false
    },
    onNodeDrag: {
      "default": function _default() {},
      triggerUpdate: false
    },
    onNodeDragEnd: {
      "default": function _default() {},
      triggerUpdate: false
    },
    onNodeClick: {
      triggerUpdate: false
    },
    onNodeRightClick: {
      triggerUpdate: false
    },
    onNodeHover: {
      triggerUpdate: false
    },
    onLinkClick: {
      triggerUpdate: false
    },
    onLinkRightClick: {
      triggerUpdate: false
    },
    onLinkHover: {
      triggerUpdate: false
    },
    onBackgroundClick: {
      triggerUpdate: false
    },
    onBackgroundRightClick: {
      triggerUpdate: false
    }
  }, linkedFGProps), linkedRenderObjsProps),
  methods: _objectSpread2(_objectSpread2({
    zoomToFit: function zoomToFit(state, transitionDuration, padding) {
      var _state$forceGraph;

      for (var _len = arguments.length, bboxArgs = new Array(_len > 3 ? _len - 3 : 0), _key = 3; _key < _len; _key++) {
        bboxArgs[_key - 3] = arguments[_key];
      }

      state.renderObjs.fitToBbox((_state$forceGraph = state.forceGraph).getGraphBbox.apply(_state$forceGraph, bboxArgs), transitionDuration, padding);
      return this;
    },
    pauseAnimation: function pauseAnimation(state) {
      if (state.animationFrameRequestId !== null) {
        cancelAnimationFrame(state.animationFrameRequestId);
        state.animationFrameRequestId = null;
      }

      return this;
    },
    resumeAnimation: function resumeAnimation(state) {
      if (state.animationFrameRequestId === null) {
        this._animationCycle();
      }

      return this;
    },
    _animationCycle: function _animationCycle(state) {
      if (state.enablePointerInteraction) {
        // reset canvas cursor (override dragControls cursor)
        //animationTick(this.renderer());
        this.renderer().domElement.style.cursor = null;
      } // Frame cycle


      state.forceGraph.tickFrame();
      state.renderObjs.tick();
      state.animationFrameRequestId = requestAnimationFrame(this._animationCycle);
    },
    scene: function scene(state) {
      return state.renderObjs.scene();
    },
    // Expose scene
    camera: function camera(state) {
      return state.renderObjs.camera();
    },
    // Expose camera
    renderer: function renderer(state) {
      return state.renderObjs.renderer();
    },
    // Expose renderer
    controls: function controls(state) {
      return state.renderObjs.controls();
    },
    // Expose controls
    tbControls: function tbControls(state) {
      return state.renderObjs.tbControls();
    },
    // To be deprecated
    _destructor: function _destructor() {
      this.pauseAnimation();
      this.graphData({
        nodes: [],
        links: []
      });
    }
  }, linkedFGMethods), linkedRenderObjsMethods),
  stateInit: function stateInit(_ref5) {
    var controlType = _ref5.controlType,
        rendererConfig = _ref5.rendererConfig,
        extraRenderers = _ref5.extraRenderers;
    return {
      forceGraph: new ThreeForceGraph(),
      renderObjs: ThreeRenderObjects({
        controlType: controlType,
        rendererConfig: rendererConfig,
        extraRenderers: extraRenderers
      })
    };
  },
  init: function init(domNode, state) {
    // Wipe DOM
    domNode.innerHTML = ''; // Add relative container

    domNode.appendChild(state.container = document.createElement('div'));
    state.container.style.position = 'relative'; // Add renderObjs

    var roDomNode = document.createElement('div');
    state.container.appendChild(roDomNode);
    state.renderObjs(roDomNode);
    var camera = state.renderObjs.camera();
    var renderer = state.renderObjs.renderer();
    var controls = state.renderObjs.controls();
    controls.enabled = !!state.enableNavigationControls;
    state.lastSetCameraZ = camera.position.z; // Add info space

    var infoElem;
    state.container.appendChild(infoElem = document.createElement('div'));
    infoElem.className = 'graph-info-msg';
    infoElem.textContent = ''; // config forcegraph

    state.forceGraph.onLoading(function () {
      infoElem.textContent = 'Loading...';
    }).onFinishLoading(function () {
      infoElem.textContent = '';
    }).onUpdate(function () {
      // sync graph data structures
      state.graphData = state.forceGraph.graphData(); // re-aim camera, if still in default position (not user modified)

      if (camera.position.x === 0 && camera.position.y === 0 && camera.position.z === state.lastSetCameraZ && state.graphData.nodes.length) {
        camera.lookAt(state.forceGraph.position);
        state.lastSetCameraZ = camera.position.z = Math.cbrt(state.graphData.nodes.length) * CAMERA_DISTANCE2NODES_FACTOR;
      }
    }).onFinishUpdate(function () {
      // Setup node drag interaction
      if (state._dragControls) {
        var curNodeDrag = state.graphData.nodes.find(function (node) {
          return node.__initialFixedPos && !node.__disposeControlsAfterDrag;
        }); // detect if there's a node being dragged using the existing drag controls

        if (curNodeDrag) {
          curNodeDrag.__disposeControlsAfterDrag = true; // postpone previous controls disposal until drag ends
        } else {
          state._dragControls.dispose(); // cancel previous drag controls

        }

        state._dragControls = undefined;
      }

      if (state.enableNodeDrag && state.enablePointerInteraction && state.forceEngine === 'd3') {
        // Can't access node positions programatically in ngraph
        var array = state.graphData.nodes.map(function (node) {
          return node.__threeObj;
        }).filter(function (obj) {
          return obj;
        });

        var dragControls = state._dragControls = new DragControls(array, camera, renderer.domElement);
        dragControls.addEventListener('dragstart', function (event) {
          controls.enabled = false; // Disable controls while dragging
          // track drag object movement

          event.object.__initialPos = event.object.position.clone();
          event.object.__prevPos = event.object.position.clone();

          var node = getGraphObj(event.object).__data;

          !node.__initialFixedPos && (node.__initialFixedPos = {
            fx: node.fx,
            fy: node.fy,
            fz: node.fz
          });
          !node.__initialPos && (node.__initialPos = {
            x: node.x,
            y: node.y,
            z: node.z
          }); // lock node

          ['x', 'y', 'z'].forEach(function (c) {
            return node["f".concat(c)] = node[c];
          }); // drag cursor

          renderer.domElement.classList.add('grabbable');
        });
        dragControls.addEventListener('drag', function (event) {
          var nodeObj = getGraphObj(event.object);

          if (!event.object.hasOwnProperty('__graphObjType')) {
            // If dragging a child of the node, update the node object instead
            var initPos = event.object.__initialPos;
            var prevPos = event.object.__prevPos;
            var _newPos = event.object.position;
            nodeObj.position.add(_newPos.clone().sub(prevPos)); // translate node object by the motion delta

            prevPos.copy(_newPos);

            _newPos.copy(initPos); // reset child back to its initial position

          }

          var node = nodeObj.__data;
          var newPos = nodeObj.position;
          var translate = {
            x: newPos.x - node.x,
            y: newPos.y - node.y,
            z: newPos.z - node.z
          }; // Move fx/fy/fz (and x/y/z) of nodes based on object new position

          ['x', 'y', 'z'].forEach(function (c) {
            return node["f".concat(c)] = node[c] = newPos[c];
          });
          state.forceGraph.d3AlphaTarget(0.3) // keep engine running at low intensity throughout drag
          .resetCountdown(); // prevent freeze while dragging

          node.__dragged = true;
          state.onNodeDrag(node, translate);
        });
        dragControls.addEventListener('dragend', function (event) {
          delete event.object.__initialPos; // remove tracking attributes

          delete event.object.__prevPos;

          var node = getGraphObj(event.object).__data; // dispose previous controls if needed


          if (node.__disposeControlsAfterDrag) {
            dragControls.dispose();
            delete node.__disposeControlsAfterDrag;
          }

          var initFixedPos = node.__initialFixedPos;
          var initPos = node.__initialPos;
          var translate = {
            x: initPos.x - node.x,
            y: initPos.y - node.y,
            z: initPos.z - node.z
          };

          if (initFixedPos) {
            ['x', 'y', 'z'].forEach(function (c) {
              var fc = "f".concat(c);

              if (initFixedPos[fc] === undefined) {
                delete node[fc];
              }
            });
            delete node.__initialFixedPos;
            delete node.__initialPos;

            if (node.__dragged) {
              delete node.__dragged;
              state.onNodeDragEnd(node, translate);
            }
          }

          state.forceGraph.d3AlphaTarget(0) // release engine low intensity
          .resetCountdown(); // let the engine readjust after releasing fixed nodes

          if (state.enableNavigationControls) {
            controls.enabled = true; // Re-enable controls

            controls.domElement && controls.domElement.ownerDocument && controls.domElement.ownerDocument.dispatchEvent( // simulate mouseup to ensure the controls don't take over after dragend
            new PointerEvent('pointerup', {
              pointerType: 'touch'
            }));
          } // clear cursor


          renderer.domElement.classList.remove('grabbable');
        });
      }
    }); // config renderObjs

    state.renderObjs.objects([// Populate scene
    new three.AmbientLight(0xbbbbbb), new three.DirectionalLight(0xffffff, 0.6), state.forceGraph]).hoverOrderComparator(function (a, b) {
      // Prioritize graph objects
      var aObj = getGraphObj(a);
      if (!aObj) return 1;
      var bObj = getGraphObj(b);
      if (!bObj) return -1; // Prioritize nodes over links

      var isNode = function isNode(o) {
        return o.__graphObjType === 'node';
      };

      return isNode(bObj) - isNode(aObj);
    }).tooltipContent(function (obj) {
      var graphObj = getGraphObj(obj);
      return graphObj ? accessorFn(state["".concat(graphObj.__graphObjType, "Label")])(graphObj.__data) || '' : '';
    }).hoverDuringDrag(false).onHover(function (obj) {
      // Update tooltip and trigger onHover events
      var hoverObj = getGraphObj(obj);

      if (hoverObj !== state.hoverObj) {
        var prevObjType = state.hoverObj ? state.hoverObj.__graphObjType : null;
        var prevObjData = state.hoverObj ? state.hoverObj.__data : null;
        var objType = hoverObj ? hoverObj.__graphObjType : null;
        var objData = hoverObj ? hoverObj.__data : null;

        if (prevObjType && prevObjType !== objType) {
          // Hover out
          var fn = state["on".concat(prevObjType === 'node' ? 'Node' : 'Link', "Hover")];
          fn && fn(null, prevObjData);
        }

        if (objType) {
          // Hover in
          var _fn = state["on".concat(objType === 'node' ? 'Node' : 'Link', "Hover")];
          _fn && _fn(objData, prevObjType === objType ? prevObjData : null);
        } // set pointer if hovered object is clickable


        renderer.domElement.classList[hoverObj && state["on".concat(objType === 'node' ? 'Node' : 'Link', "Click")] || !hoverObj && state.onBackgroundClick ? 'add' : 'remove']('clickable');
        state.hoverObj = hoverObj;
      }
    }).clickAfterDrag(false).onClick(function (obj, ev) {
      var graphObj = getGraphObj(obj);

      if (graphObj) {
        var fn = state["on".concat(graphObj.__graphObjType === 'node' ? 'Node' : 'Link', "Click")];
        fn && fn(graphObj.__data, ev);
      } else {
        state.onBackgroundClick && state.onBackgroundClick(ev);
      }
    }).onRightClick(function (obj, ev) {
      // Handle right-click events
      var graphObj = getGraphObj(obj);

      if (graphObj) {
        var fn = state["on".concat(graphObj.__graphObjType === 'node' ? 'Node' : 'Link', "RightClick")];
        fn && fn(graphObj.__data, ev);
      } else {
        state.onBackgroundRightClick && state.onBackgroundRightClick(ev);
      }
    }); //
    // Kick-off renderer

    this._animationCycle();
  }
}); //

function getGraphObj(object) {
  var obj = object; // recurse up object chain until finding the graph object

  while (obj && !obj.hasOwnProperty('__graphObjType')) {
    obj = obj.parent;
  }

  return obj;
}

export { _3dForceGraph as default };