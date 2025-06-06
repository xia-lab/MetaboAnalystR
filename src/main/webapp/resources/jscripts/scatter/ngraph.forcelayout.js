(function(f){if(typeof exports==="object"&&typeof module!=="undefined"){module.exports=f()}else if(typeof define==="function"&&define.amd){define([],f)}else{var g;if(typeof window!=="undefined"){g=window}else if(typeof global!=="undefined"){g=global}else if(typeof self!=="undefined"){g=self}else{g=this}g.ngraphCreateLayout = f()}})(function(){var define,module,exports;return (function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
module.exports = createLayout;
module.exports.simulator = require('./lib/createPhysicsSimulator');

var eventify = require('ngraph.events');

/**
 * Creates force based layout for a given graph.
 *
 * @param {ngraph.graph} graph which needs to be laid out
 * @param {object} physicsSettings if you need custom settings
 * for physics simulator you can pass your own settings here. If it's not passed
 * a default one will be created.
 */
function createLayout(graph, physicsSettings) {
  if (!graph) {
    throw new Error('Graph structure cannot be undefined');
  }

  var createSimulator = (physicsSettings && physicsSettings.createSimulator) || require('./lib/createPhysicsSimulator');
  var physicsSimulator = createSimulator(physicsSettings);
  if (Array.isArray(physicsSettings)) throw new Error('Physics settings is expected to be an object');

  // Starting from v20 of ngraph we use `Set` instead of `Array` for `node.links`
  var nodeMass = if (graph.version !== undefined && graph.version >= 20) ? defaultNodeMassWithSet : defaultNodeMass;
  if (physicsSettings && typeof physicsSettings.nodeMass === 'function') {
    nodeMass = physicsSettings.nodeMass;
  }

  var nodeBodies = new Map();
  var springs = {};
  var bodiesCount = 0;

  var springTransform = physicsSimulator.settings.springTransform || noop;

  // Initialize physics with what we have in the graph:
  initPhysics();
  listenToEvents();

  var wasStable = false;

  var api = {
    /**
     * Performs one step of iterative layout algorithm
     *
     * @returns {boolean} true if the system should be considered stable; False otherwise.
     * The system is stable if no further call to `step()` can improve the layout.
     */
    step: function() {
      if (bodiesCount === 0) {
        updateStableStatus(true);
        return true;
      }

      var lastMove = physicsSimulator.step();

      // Save the movement in case if someone wants to query it in the step
      // callback.
      api.lastMove = lastMove;

      // Allow listeners to perform low-level actions after nodes are updated.
      api.fire('step');

      var ratio = lastMove/bodiesCount;
      var isStableNow = ratio <= 0.01; // TODO: The number is somewhat arbitrary...
      updateStableStatus(isStableNow);


      return isStableNow;
    },

    /**
     * For a given `nodeId` returns position
     */
    getNodePosition: function (nodeId) {
      return getInitializedBody(nodeId).pos;
    },

    /**
     * Sets position of a node to a given coordinates
     * @param {string} nodeId node identifier
     * @param {number} x position of a node
     * @param {number} y position of a node
     * @param {number=} z position of node (only if applicable to body)
     */
    setNodePosition: function (nodeId) {
      var body = getInitializedBody(nodeId);
      body.setPosition.apply(body, Array.prototype.slice.call(arguments, 1));
    },

    /**
     * @returns {Object} Link position by link id
     * @returns {Object.from} {x, y} coordinates of link start
     * @returns {Object.to} {x, y} coordinates of link end
     */
    getLinkPosition: function (linkId) {
      var spring = springs[linkId];
      if (spring) {
        return {
          from: spring.from.pos,
          to: spring.to.pos
        };
      }
    },

    /**
     * @returns {Object} area required to fit in the graph. Object contains
     * `x1`, `y1` - top left coordinates
     * `x2`, `y2` - bottom right coordinates
     */
    getGraphRect: function () {
      return physicsSimulator.getBBox();
    },

    /**
     * Iterates over each body in the layout simulator and performs a callback(body, nodeId)
     */
    forEachBody: forEachBody,

    /*
     * Requests layout algorithm to pin/unpin node to its current position
     * Pinned nodes should not be affected by layout algorithm and always
     * remain at their position
     */
    pinNode: function (node, isPinned) {
      var body = getInitializedBody(node.id);
       body.isPinned = !!isPinned;
    },

    /**
     * Checks whether given graph's node is currently pinned
     */
    isNodePinned: function (node) {
      return getInitializedBody(node.id).isPinned;
    },

    /**
     * Request to release all resources
     */
    dispose: function() {
      graph.off('changed', onGraphChanged);
      api.fire('disposed');
    },

    /**
     * Gets physical body for a given node id. If node is not found undefined
     * value is returned.
     */
    getBody: getBody,

    /**
     * Gets spring for a given edge.
     *
     * @param {string} linkId link identifer. If two arguments are passed then
     * this argument is treated as formNodeId
     * @param {string=} toId when defined this parameter denotes head of the link
     * and first argument is treated as tail of the link (fromId)
     */
    getSpring: getSpring,

    /**
     * Returns length of cumulative force vector. The closer this to zero - the more stable the system is
     */
    getForceVectorLength: getForceVectorLength,

    /**
     * [Read only] Gets current physics simulator
     */
    simulator: physicsSimulator,

    /**
     * Gets the graph that was used for layout
     */
    graph: graph,

    /**
     * Gets amount of movement performed during last step operation
     */
    lastMove: 0
  };

  eventify(api);

  return api;

  function updateStableStatus(isStableNow) {
    if (wasStable !== isStableNow) {
      wasStable = isStableNow;
      onStableChanged(isStableNow);
    }
  }

  function forEachBody(cb) {
    nodeBodies.forEach(cb);
  }

  function getForceVectorLength() {
    var fx = 0, fy = 0;
    forEachBody(function(body) {
      fx += Math.abs(body.force.x);
      fy += Math.abs(body.force.y);
    });
    return Math.sqrt(fx * fx + fy * fy);
  }

  function getSpring(fromId, toId) {
    var linkId;
    if (toId === undefined) {
      if (typeof fromId !== 'object') {
        // assume fromId as a linkId:
        linkId = fromId;
      } else {
        // assume fromId to be a link object:
        linkId = fromId.id;
      }
    } else {
      // toId is defined, should grab link:
      var link = graph.hasLink(fromId, toId);
      if (!link) return;
      linkId = link.id;
    }

    return springs[linkId];
  }

  function getBody(nodeId) {
    return nodeBodies.get(nodeId);
  }

  function listenToEvents() {
    graph.on('changed', onGraphChanged);
  }

  function onStableChanged(isStable) {
    api.fire('stable', isStable);
  }

  function onGraphChanged(changes) {
    for (var i = 0; i < changes.length; ++i) {
      var change = changes[i];
      if (change.changeType === 'add') {
        if (change.node) {
          initBody(change.node.id);
        }
        if (change.link) {
          initLink(change.link);
        }
      } else if (change.changeType === 'remove') {
        if (change.node) {
          releaseNode(change.node);
        }
        if (change.link) {
          releaseLink(change.link);
        }
      }
    }
    bodiesCount = graph.getNodesCount();
  }

  function initPhysics() {
    bodiesCount = 0;

    graph.forEachNode(function (node) {
      initBody(node.id);
      bodiesCount += 1;
    });

    graph.forEachLink(initLink);
  }

  function initBody(nodeId) {
    var body = nodeBodies.get(nodeId);
    if (!body) {
      var node = graph.getNode(nodeId);
      if (!node) {
        throw new Error('initBody() was called with unknown node id');
      }

      var pos = node.position;
      if (!pos) {
        var neighbors = getNeighborBodies(node);
        pos = physicsSimulator.getBestNewBodyPosition(neighbors);
      }

      body = physicsSimulator.addBodyAt(pos);
      body.id = nodeId;

      nodeBodies.set(nodeId, body);
      updateBodyMass(nodeId);

      if (isNodeOriginallyPinned(node)) {
        body.isPinned = true;
      }
    }
  }

  function releaseNode(node) {
    var nodeId = node.id;
    var body = nodeBodies.get(nodeId);
    if (body) {
      nodeBodies.delete(nodeId);
      physicsSimulator.removeBody(body);
    }
  }

  function initLink(link) {
    updateBodyMass(link.fromId);
    updateBodyMass(link.toId);

    var fromBody = nodeBodies.get(link.fromId),
        toBody  = nodeBodies.get(link.toId),
        spring = physicsSimulator.addSpring(fromBody, toBody, link.length);

    springTransform(link, spring);

    springs[link.id] = spring;
  }

  function releaseLink(link) {
    var spring = springs[link.id];
    if (spring) {
      var from = graph.getNode(link.fromId),
          to = graph.getNode(link.toId);

      if (from) updateBodyMass(from.id);
      if (to) updateBodyMass(to.id);

      delete springs[link.id];

      physicsSimulator.removeSpring(spring);
    }
  }

  function getNeighborBodies(node) {
    // TODO: Could probably be done better on memory
    var neighbors = [];
    if (!node.links) {
      return neighbors;
    }
    var maxNeighbors = Math.min(node.links.length, 2);
    for (var i = 0; i < maxNeighbors; ++i) {
      var link = node.links[i];
      var otherBody = link.fromId !== node.id ? nodeBodies.get(link.fromId) : nodeBodies.get(link.toId);
      if (otherBody && otherBody.pos) {
        neighbors.push(otherBody);
      }
    }

    return neighbors;
  }

  function updateBodyMass(nodeId) {
    var body = nodeBodies.get(nodeId);
    body.mass = nodeMass(nodeId);
    if (Number.isNaN(body.mass)) {
      throw new Error('Node mass should be a number');
    }
  }

  /**
   * Checks whether graph node has in its settings pinned attribute,
   * which means layout algorithm cannot move it. Node can be marked
   * as pinned, if it has "isPinned" attribute, or when node.data has it.
   *
   * @param {Object} node a graph node to check
   * @return {Boolean} true if node should be treated as pinned; false otherwise.
   */
  function isNodeOriginallyPinned(node) {
    return (node && (node.isPinned || (node.data && node.data.isPinned)));
  }

  function getInitializedBody(nodeId) {
    var body = nodeBodies.get(nodeId);
    if (!body) {
      initBody(nodeId);
      body = nodeBodies.get(nodeId);
    }
    return body;
  }

  /**
   * Calculates mass of a body, which corresponds to node with given id.
   *
   * @param {String|Number} nodeId identifier of a node, for which body mass needs to be calculated
   * @returns {Number} recommended mass of the body;
   */
  function defaultNodeMass(nodeId) {
    var links = graph.getLinks(nodeId);
    if (!links) return 1;
    return 1 + links.length / 3.0;
  }

  function defaultNodeMassWithSet(nodeId) {
    var links = graph.getLinks(nodeId);
    if (!links) return 1;
    return 1 + links.size / 3.0;
  }
}

function noop() { }

},{"./lib/createPhysicsSimulator":10,"ngraph.events":12}],2:[function(require,module,exports){
const getVariableName = require('./getVariableName');

module.exports = function createPatternBuilder(dimension) {

  return pattern;
  
  function pattern(template, config) {
    let indent = (config && config.indent) || 0;
    let join = (config && config.join !== undefined) ? config.join : '\n';
    let indentString = Array(indent + 1).join(' ');
    let buffer = [];
    for (let i = 0; i < dimension; ++i) {
      let variableName = getVariableName(i);
      let prefix = (i === 0) ? '' : indentString;
      buffer.push(prefix + template.replace(/{var}/g, variableName));
    }
    return buffer.join(join);
  }
};

},{"./getVariableName":9}],3:[function(require,module,exports){

module.exports = generateBoundsFunction;
module.exports.generateFunctionBody = generateBoundsFunctionBody;

const createPatternBuilder = require('./createPatternBuilder');

function generateBoundsFunction(dimension) {
  let code = generateBoundsFunctionBody(dimension);
  return new Function('bodies', 'settings', 'random', code);
}

function generateBoundsFunctionBody(dimension) {
  let pattern = createPatternBuilder(dimension);

  let code = `
  var boundingBox = {
    ${pattern('min_{var}: 0, max_{var}: 0,', {indent: 4})}
  };

  return {
    box: boundingBox,

    update: updateBoundingBox,

    reset: resetBoundingBox,

    getBestNewPosition: function (neighbors) {
      var ${pattern('base_{var} = 0', {join: ', '})};

      if (neighbors.length) {
        for (var i = 0; i < neighbors.length; ++i) {
          let neighborPos = neighbors[i].pos;
          ${pattern('base_{var} += neighborPos.{var};', {indent: 10})}
        }

        ${pattern('base_{var} /= neighbors.length;', {indent: 8})}
      } else {
        ${pattern('base_{var} = (boundingBox.min_{var} + boundingBox.max_{var}) / 2;', {indent: 8})}
      }

      var springLength = settings.springLength;
      return {
        ${pattern('{var}: base_{var} + (random.nextDouble() - 0.5) * springLength,', {indent: 8})}
      };
    }
  };

  function updateBoundingBox() {
    var i = bodies.length;
    if (i === 0) return; // No bodies - no borders.

    ${pattern('var max_{var} = -Infinity;', {indent: 4})}
    ${pattern('var min_{var} = Infinity;', {indent: 4})}

    while(i--) {
      // this is O(n), it could be done faster with quadtree, if we check the root node bounds
      var bodyPos = bodies[i].pos;
      ${pattern('if (bodyPos.{var} < min_{var}) min_{var} = bodyPos.{var};', {indent: 6})}
      ${pattern('if (bodyPos.{var} > max_{var}) max_{var} = bodyPos.{var};', {indent: 6})}
    }

    ${pattern('boundingBox.min_{var} = min_{var};', {indent: 4})}
    ${pattern('boundingBox.max_{var} = max_{var};', {indent: 4})}
  }

  function resetBoundingBox() {
    ${pattern('boundingBox.min_{var} = boundingBox.max_{var} = 0;', {indent: 4})}
  }
`;
  return code;
}

},{"./createPatternBuilder":2}],4:[function(require,module,exports){

const createPatternBuilder = require('./createPatternBuilder');

module.exports = generateCreateBodyFunction;
module.exports.generateCreateBodyFunctionBody = generateCreateBodyFunctionBody;

// InlineTransform: getVectorCode
module.exports.getVectorCode = getVectorCode;
// InlineTransform: getBodyCode
module.exports.getBodyCode = getBodyCode;
// InlineTransformExport: module.exports = function() { return Body; }

function generateCreateBodyFunction(dimension, debugSetters) {
  let code = generateCreateBodyFunctionBody(dimension, debugSetters);
  let {Body} = (new Function(code))();
  return Body;
}

function generateCreateBodyFunctionBody(dimension, debugSetters) {
  let code = `
${getVectorCode(dimension, debugSetters)}
${getBodyCode(dimension, debugSetters)}
return {Body: Body, Vector: Vector};
`;
  return code;
}

function getBodyCode(dimension) {
  let pattern = createPatternBuilder(dimension);
  let variableList = pattern('{var}', {join: ', '});
  return `
function Body(${variableList}) {
  this.isPinned = false;
  this.pos = new Vector(${variableList});
  this.force = new Vector();
  this.velocity = new Vector();
  this.mass = 1;

  this.springCount = 0;
  this.springLength = 0;
}

Body.prototype.reset = function() {
  this.force.reset();
  this.springCount = 0;
  this.springLength = 0;
}

Body.prototype.setPosition = function (${variableList}) {
  ${pattern('this.pos.{var} = {var} || 0;', {indent: 2})}
};`;
}

function getVectorCode(dimension, debugSetters) {
  let pattern = createPatternBuilder(dimension);
  let setters = '';
  if (debugSetters) {
    setters = `${pattern("\n\
   var v{var};\n\
Object.defineProperty(this, '{var}', {\n\
  set: function(v) { \n\
    if (!Number.isFinite(v)) throw new Error('Cannot set non-numbers to {var}');\n\
    v{var} = v; \n\
  },\n\
  get: function() { return v{var}; }\n\
});")}`;
  }

  let variableList = pattern('{var}', {join: ', '});
  return `function Vector(${variableList}) {
  ${setters}
    if (typeof arguments[0] === 'object') {
      // could be another vector
      let v = arguments[0];
      ${pattern('if (!Number.isFinite(v.{var})) throw new Error("Expected value is not a finite number at Vector constructor ({var})");', {indent: 4})}
      ${pattern('this.{var} = v.{var};', {indent: 4})}
    } else {
      ${pattern('this.{var} = typeof {var} === "number" ? {var} : 0;', {indent: 4})}
    }
  }
  
  Vector.prototype.reset = function () {
    ${pattern('this.{var} = ', {join: ''})}0;
  };`;
}
},{"./createPatternBuilder":2}],5:[function(require,module,exports){
const createPatternBuilder = require('./createPatternBuilder');

module.exports = generateCreateDragForceFunction;
module.exports.generateCreateDragForceFunctionBody = generateCreateDragForceFunctionBody;

function generateCreateDragForceFunction(dimension) {
  let code = generateCreateDragForceFunctionBody(dimension);
  return new Function('options', code);
}

function generateCreateDragForceFunctionBody(dimension) {
  let pattern = createPatternBuilder(dimension);
  let code = `
  if (!Number.isFinite(options.dragCoefficient)) throw new Error('dragCoefficient is not a finite number');

  return {
    update: function(body) {
      ${pattern('body.force.{var} -= options.dragCoefficient * body.velocity.{var};', {indent: 6})}
    }
  };
`;
  return code;
}

},{"./createPatternBuilder":2}],6:[function(require,module,exports){
const createPatternBuilder = require('./createPatternBuilder');

module.exports = generateCreateSpringForceFunction;
module.exports.generateCreateSpringForceFunctionBody = generateCreateSpringForceFunctionBody;

function generateCreateSpringForceFunction(dimension) {
  let code = generateCreateSpringForceFunctionBody(dimension);
  return new Function('options', 'random', code);
}

function generateCreateSpringForceFunctionBody(dimension) {
  let pattern = createPatternBuilder(dimension);
  let code = `
  if (!Number.isFinite(options.springCoefficient)) throw new Error('Spring coefficient is not a number');
  if (!Number.isFinite(options.springLength)) throw new Error('Spring length is not a number');

  return {
    /**
     * Updates forces acting on a spring
     */
    update: function (spring) {
      var body1 = spring.from;
      var body2 = spring.to;
      var length = spring.length < 0 ? options.springLength : spring.length;
      ${pattern('var d{var} = body2.pos.{var} - body1.pos.{var};', {indent: 6})}
      var r = Math.sqrt(${pattern('d{var} * d{var}', {join: ' + '})});

      if (r === 0) {
        ${pattern('d{var} = (random.nextDouble() - 0.5) / 50;', {indent: 8})}
        r = Math.sqrt(${pattern('d{var} * d{var}', {join: ' + '})});
      }

      var d = r - length;
      var coefficient = ((spring.coefficient > 0) ? spring.coefficient : options.springCoefficient) * d / r;

      ${pattern('body1.force.{var} += coefficient * d{var}', {indent: 6})};
      body1.springCount += 1;
      body1.springLength += r;

      ${pattern('body2.force.{var} -= coefficient * d{var}', {indent: 6})};
      body2.springCount += 1;
      body2.springLength += r;
    }
  };
`;
  return code;
}

},{"./createPatternBuilder":2}],7:[function(require,module,exports){
const createPatternBuilder = require('./createPatternBuilder');

module.exports = generateIntegratorFunction;
module.exports.generateIntegratorFunctionBody = generateIntegratorFunctionBody;

function generateIntegratorFunction(dimension) {
  let code = generateIntegratorFunctionBody(dimension);
  return new Function('bodies', 'timeStep', 'adaptiveTimeStepWeight', code);
}

function generateIntegratorFunctionBody(dimension) {
  let pattern = createPatternBuilder(dimension);
  let code = `
  var length = bodies.length;
  if (length === 0) return 0;

  ${pattern('var d{var} = 0, t{var} = 0;', {indent: 2})}

  for (var i = 0; i < length; ++i) {
    var body = bodies[i];
    if (body.isPinned) continue;

    if (adaptiveTimeStepWeight && body.springCount) {
      timeStep = (adaptiveTimeStepWeight * body.springLength/body.springCount);
    }

    var coeff = timeStep / body.mass;

    ${pattern('body.velocity.{var} += coeff * body.force.{var};', {indent: 4})}
    ${pattern('var v{var} = body.velocity.{var};', {indent: 4})}
    var v = Math.sqrt(${pattern('v{var} * v{var}', {join: ' + '})});

    if (v > 1) {
      // We normalize it so that we move within timeStep range. 
      // for the case when v <= 1 - we let velocity to fade out.
      ${pattern('body.velocity.{var} = v{var} / v;', {indent: 6})}
    }

    ${pattern('d{var} = timeStep * body.velocity.{var};', {indent: 4})}

    ${pattern('body.pos.{var} += d{var};', {indent: 4})}

    ${pattern('t{var} += Math.abs(d{var});', {indent: 4})}
  }

  return (${pattern('t{var} * t{var}', {join: ' + '})})/length;
`;
  return code;
}

},{"./createPatternBuilder":2}],8:[function(require,module,exports){
const createPatternBuilder = require('./createPatternBuilder');
const getVariableName = require('./getVariableName');

module.exports = generateQuadTreeFunction;
module.exports.generateQuadTreeFunctionBody = generateQuadTreeFunctionBody;

// These exports are for InlineTransform tool.
// InlineTransform: getInsertStackCode
module.exports.getInsertStackCode = getInsertStackCode;
// InlineTransform: getQuadNodeCode
module.exports.getQuadNodeCode = getQuadNodeCode;
// InlineTransform: isSamePosition
module.exports.isSamePosition = isSamePosition;
// InlineTransform: getChildBodyCode
module.exports.getChildBodyCode = getChildBodyCode;
// InlineTransform: setChildBodyCode
module.exports.setChildBodyCode = setChildBodyCode;

function generateQuadTreeFunction(dimension) {
  let code = generateQuadTreeFunctionBody(dimension);
  return (new Function(code))();
}

function generateQuadTreeFunctionBody(dimension) {
  let pattern = createPatternBuilder(dimension);
  let quadCount = Math.pow(2, dimension);

  let code = `
${getInsertStackCode()}
${getQuadNodeCode(dimension)}
${isSamePosition(dimension)}
${getChildBodyCode(dimension)}
${setChildBodyCode(dimension)}

function createQuadTree(options, random) {
  options = options || {};
  options.gravity = typeof options.gravity === 'number' ? options.gravity : -1;
  options.theta = typeof options.theta === 'number' ? options.theta : 0.8;

  var gravity = options.gravity;
  var updateQueue = [];
  var insertStack = new InsertStack();
  var theta = options.theta;

  var nodesCache = [];
  var currentInCache = 0;
  var root = newNode();

  return {
    insertBodies: insertBodies,

    /**
     * Gets root node if it is present
     */
    getRoot: function() {
      return root;
    },

    updateBodyForce: update,

    options: function(newOptions) {
      if (newOptions) {
        if (typeof newOptions.gravity === 'number') {
          gravity = newOptions.gravity;
        }
        if (typeof newOptions.theta === 'number') {
          theta = newOptions.theta;
        }

        return this;
      }

      return {
        gravity: gravity,
        theta: theta
      };
    }
  };

  function newNode() {
    // To avoid pressure on GC we reuse nodes.
    var node = nodesCache[currentInCache];
    if (node) {
${assignQuads('      node.')}
      node.body = null;
      node.mass = ${pattern('node.mass_{var} = ', {join: ''})}0;
      ${pattern('node.min_{var} = node.max_{var} = ', {join: ''})}0;
    } else {
      node = new QuadNode();
      nodesCache[currentInCache] = node;
    }

    ++currentInCache;
    return node;
  }

  function update(sourceBody) {
    var queue = updateQueue;
    var v;
    ${pattern('var d{var};', {indent: 4})}
    var r; 
    ${pattern('var f{var} = 0;', {indent: 4})}
    var queueLength = 1;
    var shiftIdx = 0;
    var pushIdx = 1;

    queue[0] = root;

    while (queueLength) {
      var node = queue[shiftIdx];
      var body = node.body;

      queueLength -= 1;
      shiftIdx += 1;
      var differentBody = (body !== sourceBody);
      if (body && differentBody) {
        // If the current node is a leaf node (and it is not source body),
        // calculate the force exerted by the current node on body, and add this
        // amount to body's net force.
        ${pattern('d{var} = body.pos.{var} - sourceBody.pos.{var};', {indent: 8})}
        r = Math.sqrt(${pattern('d{var} * d{var}', {join: ' + '})});

        if (r === 0) {
          // Poor man's protection against zero distance.
          ${pattern('d{var} = (random.nextDouble() - 0.5) / 50;', {indent: 10})}
          r = Math.sqrt(${pattern('d{var} * d{var}', {join: ' + '})});
        }

        // This is standard gravitation force calculation but we divide
        // by r^3 to save two operations when normalizing force vector.
        v = gravity * body.mass * sourceBody.mass / (r * r * r);
        ${pattern('f{var} += v * d{var};', {indent: 8})}
      } else if (differentBody) {
        // Otherwise, calculate the ratio s / r,  where s is the width of the region
        // represented by the internal node, and r is the distance between the body
        // and the node's center-of-mass
        ${pattern('d{var} = node.mass_{var} / node.mass - sourceBody.pos.{var};', {indent: 8})}
        r = Math.sqrt(${pattern('d{var} * d{var}', {join: ' + '})});

        if (r === 0) {
          // Sorry about code duplication. I don't want to create many functions
          // right away. Just want to see performance first.
          ${pattern('d{var} = (random.nextDouble() - 0.5) / 50;', {indent: 10})}
          r = Math.sqrt(${pattern('d{var} * d{var}', {join: ' + '})});
        }
        // If s / r < θ, treat this internal node as a single body, and calculate the
        // force it exerts on sourceBody, and add this amount to sourceBody's net force.
        if ((node.max_${getVariableName(0)} - node.min_${getVariableName(0)}) / r < theta) {
          // in the if statement above we consider node's width only
          // because the region was made into square during tree creation.
          // Thus there is no difference between using width or height.
          v = gravity * node.mass * sourceBody.mass / (r * r * r);
          ${pattern('f{var} += v * d{var};', {indent: 10})}
        } else {
          // Otherwise, run the procedure recursively on each of the current node's children.

          // I intentionally unfolded this loop, to save several CPU cycles.
${runRecursiveOnChildren()}
        }
      }
    }

    ${pattern('sourceBody.force.{var} += f{var};', {indent: 4})}
  }

  function insertBodies(bodies) {
    ${pattern('var {var}min = Number.MAX_VALUE;', {indent: 4})}
    ${pattern('var {var}max = Number.MIN_VALUE;', {indent: 4})}
    var i = bodies.length;

    // To reduce quad tree depth we are looking for exact bounding box of all particles.
    while (i--) {
      var pos = bodies[i].pos;
      ${pattern('if (pos.{var} < {var}min) {var}min = pos.{var};', {indent: 6})}
      ${pattern('if (pos.{var} > {var}max) {var}max = pos.{var};', {indent: 6})}
    }

    // Makes the bounds square.
    var maxSideLength = -Infinity;
    ${pattern('if ({var}max - {var}min > maxSideLength) maxSideLength = {var}max - {var}min ;', {indent: 4})}

    currentInCache = 0;
    root = newNode();
    ${pattern('root.min_{var} = {var}min;', {indent: 4})}
    ${pattern('root.max_{var} = {var}min + maxSideLength;', {indent: 4})}

    i = bodies.length - 1;
    if (i >= 0) {
      root.body = bodies[i];
    }
    while (i--) {
      insert(bodies[i], root);
    }
  }

  function insert(newBody) {
    insertStack.reset();
    insertStack.push(root, newBody);

    while (!insertStack.isEmpty()) {
      var stackItem = insertStack.pop();
      var node = stackItem.node;
      var body = stackItem.body;

      if (!node.body) {
        // This is internal node. Update the total mass of the node and center-of-mass.
        ${pattern('var {var} = body.pos.{var};', {indent: 8})}
        node.mass += body.mass;
        ${pattern('node.mass_{var} += body.mass * {var};', {indent: 8})}

        // Recursively insert the body in the appropriate quadrant.
        // But first find the appropriate quadrant.
        var quadIdx = 0; // Assume we are in the 0's quad.
        ${pattern('var min_{var} = node.min_{var};', {indent: 8})}
        ${pattern('var max_{var} = (min_{var} + node.max_{var}) / 2;', {indent: 8})}

${assignInsertionQuadIndex(8)}

        var child = getChild(node, quadIdx);

        if (!child) {
          // The node is internal but this quadrant is not taken. Add
          // subnode to it.
          child = newNode();
          ${pattern('child.min_{var} = min_{var};', {indent: 10})}
          ${pattern('child.max_{var} = max_{var};', {indent: 10})}
          child.body = body;

          setChild(node, quadIdx, child);
        } else {
          // continue searching in this quadrant.
          insertStack.push(child, body);
        }
      } else {
        // We are trying to add to the leaf node.
        // We have to convert current leaf into internal node
        // and continue adding two nodes.
        var oldBody = node.body;
        node.body = null; // internal nodes do not cary bodies

        if (isSamePosition(oldBody.pos, body.pos)) {
          // Prevent infinite subdivision by bumping one node
          // anywhere in this quadrant
          var retriesCount = 3;
          do {
            var offset = random.nextDouble();
            ${pattern('var d{var} = (node.max_{var} - node.min_{var}) * offset;', {indent: 12})}

            ${pattern('oldBody.pos.{var} = node.min_{var} + d{var};', {indent: 12})}
            retriesCount -= 1;
            // Make sure we don't bump it out of the box. If we do, next iteration should fix it
          } while (retriesCount > 0 && isSamePosition(oldBody.pos, body.pos));

          if (retriesCount === 0 && isSamePosition(oldBody.pos, body.pos)) {
            // This is very bad, we ran out of precision.
            // if we do not return from the method we'll get into
            // infinite loop here. So we sacrifice correctness of layout, and keep the app running
            // Next layout iteration should get larger bounding box in the first step and fix this
            return;
          }
        }
        // Next iteration should subdivide node further.
        insertStack.push(node, oldBody);
        insertStack.push(node, body);
      }
    }
  }
}
return createQuadTree;

`;
  return code;


  function assignInsertionQuadIndex(indentCount) {
    let insertionCode = [];
    let indent = Array(indentCount + 1).join(' ');
    for (let i = 0; i < dimension; ++i) {
      insertionCode.push(indent + `if (${getVariableName(i)} > max_${getVariableName(i)}) {`);
      insertionCode.push(indent + `  quadIdx = quadIdx + ${Math.pow(2, i)};`);
      insertionCode.push(indent + `  min_${getVariableName(i)} = max_${getVariableName(i)};`);
      insertionCode.push(indent + `  max_${getVariableName(i)} = node.max_${getVariableName(i)};`);
      insertionCode.push(indent + `}`);
    }
    return insertionCode.join('\n');
    // if (x > max_x) { // somewhere in the eastern part.
    //   quadIdx = quadIdx + 1;
    //   left = right;
    //   right = node.right;
    // }
  }

  function runRecursiveOnChildren() {
    let indent = Array(11).join(' ');
    let recursiveCode = [];
    for (let i = 0; i < quadCount; ++i) {
      recursiveCode.push(indent + `if (node.quad${i}) {`);
      recursiveCode.push(indent + `  queue[pushIdx] = node.quad${i};`);
      recursiveCode.push(indent + `  queueLength += 1;`);
      recursiveCode.push(indent + `  pushIdx += 1;`);
      recursiveCode.push(indent + `}`);
    }
    return recursiveCode.join('\n');
    // if (node.quad0) {
    //   queue[pushIdx] = node.quad0;
    //   queueLength += 1;
    //   pushIdx += 1;
    // }
  }

  function assignQuads(indent) {
    // this.quad0 = null;
    // this.quad1 = null;
    // this.quad2 = null;
    // this.quad3 = null;
    let quads = [];
    for (let i = 0; i < quadCount; ++i) {
      quads.push(`${indent}quad${i} = null;`);
    }
    return quads.join('\n');
  }
}

function isSamePosition(dimension) {
  let pattern = createPatternBuilder(dimension);
  return `
  function isSamePosition(point1, point2) {
    ${pattern('var d{var} = Math.abs(point1.{var} - point2.{var});', {indent: 2})}
  
    return ${pattern('d{var} < 1e-8', {join: ' && '})};
  }  
`;
}

function setChildBodyCode(dimension) {
  var quadCount = Math.pow(2, dimension);
  return `
function setChild(node, idx, child) {
  ${setChildBody()}
}`;
  function setChildBody() {
    let childBody = [];
    for (let i = 0; i < quadCount; ++i) {
      let prefix = (i === 0) ? '  ' : '  else ';
      childBody.push(`${prefix}if (idx === ${i}) node.quad${i} = child;`);
    }

    return childBody.join('\n');
    // if (idx === 0) node.quad0 = child;
    // else if (idx === 1) node.quad1 = child;
    // else if (idx === 2) node.quad2 = child;
    // else if (idx === 3) node.quad3 = child;
  }
}

function getChildBodyCode(dimension) {
  return `function getChild(node, idx) {
${getChildBody()}
  return null;
}`;

  function getChildBody() {
    let childBody = [];
    let quadCount = Math.pow(2, dimension);
    for (let i = 0; i < quadCount; ++i) {
      childBody.push(`  if (idx === ${i}) return node.quad${i};`);
    }

    return childBody.join('\n');
    // if (idx === 0) return node.quad0;
    // if (idx === 1) return node.quad1;
    // if (idx === 2) return node.quad2;
    // if (idx === 3) return node.quad3;
  }
}

function getQuadNodeCode(dimension) {
  let pattern = createPatternBuilder(dimension);
  let quadCount = Math.pow(2, dimension);
  var quadNodeCode = `
function QuadNode() {
  // body stored inside this node. In quad tree only leaf nodes (by construction)
  // contain bodies:
  this.body = null;

  // Child nodes are stored in quads. Each quad is presented by number:
  // 0 | 1
  // -----
  // 2 | 3
${assignQuads('  this.')}

  // Total mass of current node
  this.mass = 0;

  // Center of mass coordinates
  ${pattern('this.mass_{var} = 0;', {indent: 2})}

  // bounding box coordinates
  ${pattern('this.min_{var} = 0;', {indent: 2})}
  ${pattern('this.max_{var} = 0;', {indent: 2})}
}
`;
  return quadNodeCode;

  function assignQuads(indent) {
    // this.quad0 = null;
    // this.quad1 = null;
    // this.quad2 = null;
    // this.quad3 = null;
    let quads = [];
    for (let i = 0; i < quadCount; ++i) {
      quads.push(`${indent}quad${i} = null;`);
    }
    return quads.join('\n');
  }
}

function getInsertStackCode() {
  return `
/**
 * Our implementation of QuadTree is non-recursive to avoid GC hit
 * This data structure represent stack of elements
 * which we are trying to insert into quad tree.
 */
function InsertStack () {
    this.stack = [];
    this.popIdx = 0;
}

InsertStack.prototype = {
    isEmpty: function() {
        return this.popIdx === 0;
    },
    push: function (node, body) {
        var item = this.stack[this.popIdx];
        if (!item) {
            // we are trying to avoid memory pressure: create new element
            // only when absolutely necessary
            this.stack[this.popIdx] = new InsertStackElement(node, body);
        } else {
            item.node = node;
            item.body = body;
        }
        ++this.popIdx;
    },
    pop: function () {
        if (this.popIdx > 0) {
            return this.stack[--this.popIdx];
        }
    },
    reset: function () {
        this.popIdx = 0;
    }
};

function InsertStackElement(node, body) {
    this.node = node; // QuadTree node
    this.body = body; // physical body which needs to be inserted to node
}
`;
}
},{"./createPatternBuilder":2,"./getVariableName":9}],9:[function(require,module,exports){
module.exports = function getVariableName(index) {
  if (index === 0) return 'x';
  if (index === 1) return 'y';
  if (index === 2) return 'z';
  return 'c' + (index + 1);
};
},{}],10:[function(require,module,exports){
/**
 * Manages a simulation of physical forces acting on bodies and springs.
 */
module.exports = createPhysicsSimulator;

var generateCreateBodyFunction = require('./codeGenerators/generateCreateBody');
var generateQuadTreeFunction = require('./codeGenerators/generateQuadTree');
var generateBoundsFunction = require('./codeGenerators/generateBounds');
var generateCreateDragForceFunction = require('./codeGenerators/generateCreateDragForce');
var generateCreateSpringForceFunction = require('./codeGenerators/generateCreateSpringForce');
var generateIntegratorFunction = require('./codeGenerators/generateIntegrator');

var dimensionalCache = {};

function createPhysicsSimulator(settings) {
  var Spring = require('./spring');
  var merge = require('ngraph.merge');
  var eventify = require('ngraph.events');
  if (settings) {
    // Check for names from older versions of the layout
    if (settings.springCoeff !== undefined) throw new Error('springCoeff was renamed to springCoefficient');
    if (settings.dragCoeff !== undefined) throw new Error('dragCoeff was renamed to dragCoefficient');
  }

  settings = merge(settings, {
      /**
       * Ideal length for links (springs in physical model).
       */
      springLength: 10,

      /**
       * Hook's law coefficient. 1 - solid spring.
       */
      springCoefficient: 0.8, 

      /**
       * Coulomb's law coefficient. It's used to repel nodes thus should be negative
       * if you make it positive nodes start attract each other :).
       */
      gravity: -12,

      /**
       * Theta coefficient from Barnes Hut simulation. Ranged between (0, 1).
       * The closer it's to 1 the more nodes algorithm will have to go through.
       * Setting it to one makes Barnes Hut simulation no different from
       * brute-force forces calculation (each node is considered).
       */
      theta: 0.8,

      /**
       * Drag force coefficient. Used to slow down system, thus should be less than 1.
       * The closer it is to 0 the less tight system will be.
       */
      dragCoefficient: 0.9, // TODO: Need to rename this to something better. E.g. `dragCoefficient`

      /**
       * Default time step (dt) for forces integration
       */
      timeStep : 0.5,

      /**
       * Adaptive time step uses average spring length to compute actual time step:
       * See: https://twitter.com/anvaka/status/1293067160755957760
       */
      adaptiveTimeStepWeight: 0,

      /**
       * This parameter defines number of dimensions of the space where simulation
       * is performed. 
       */
      dimensions: 2,

      /**
       * In debug mode more checks are performed, this will help you catch errors
       * quickly, however for production build it is recommended to turn off this flag
       * to speed up computation.
       */
      debug: false
  });

  var factory = dimensionalCache[settings.dimensions];
  if (!factory) {
    var dimensions = settings.dimensions;
    factory = {
      Body: generateCreateBodyFunction(dimensions, settings.debug),
      createQuadTree: generateQuadTreeFunction(dimensions),
      createBounds: generateBoundsFunction(dimensions),
      createDragForce: generateCreateDragForceFunction(dimensions),
      createSpringForce: generateCreateSpringForceFunction(dimensions),
      integrate: generateIntegratorFunction(dimensions),
    };
    dimensionalCache[dimensions] = factory;
  }

  var Body = factory.Body;
  var createQuadTree = factory.createQuadTree;
  var createBounds = factory.createBounds;
  var createDragForce = factory.createDragForce;
  var createSpringForce = factory.createSpringForce;
  var integrate = factory.integrate;
  var createBody = pos => new Body(pos);

  var random = require('ngraph.random').random(42);
  var bodies = []; // Bodies in this simulation.
  var springs = []; // Springs in this simulation.

  var quadTree = createQuadTree(settings, random);
  var bounds = createBounds(bodies, settings, random);
  var springForce = createSpringForce(settings, random);
  var dragForce = createDragForce(settings);

  var totalMovement = 0; // how much movement we made on last step
  var forces = [];
  var forceMap = new Map();
  var iterationNumber = 0;
 
  addForce('nbody', nbodyForce);
  addForce('spring', updateSpringForce);

  var publicApi = {
    /**
     * Array of bodies, registered with current simulator
     *
     * Note: To add new body, use addBody() method. This property is only
     * exposed for testing/performance purposes.
     */
    bodies: bodies,
  
    quadTree: quadTree,

    /**
     * Array of springs, registered with current simulator
     *
     * Note: To add new spring, use addSpring() method. This property is only
     * exposed for testing/performance purposes.
     */
    springs: springs,

    /**
     * Returns settings with which current simulator was initialized
     */
    settings: settings,

    /**
     * Adds a new force to simulation
     */
    addForce: addForce,
    
    /**
     * Removes a force from the simulation.
     */
    removeForce: removeForce,

    /**
     * Returns a map of all registered forces.
     */
    getForces: getForces,

    /**
     * Performs one step of force simulation.
     *
     * @returns {boolean} true if system is considered stable; False otherwise.
     */
    step: function () {
      for (var i = 0; i < forces.length; ++i) {
        forces[i](iterationNumber);
      }
      var movement = integrate(bodies, settings.timeStep, settings.adaptiveTimeStepWeight);
      iterationNumber += 1;
      return movement;
    },

    /**
     * Adds body to the system
     *
     * @param {ngraph.physics.primitives.Body} body physical body
     *
     * @returns {ngraph.physics.primitives.Body} added body
     */
    addBody: function (body) {
      if (!body) {
        throw new Error('Body is required');
      }
      bodies.push(body);

      return body;
    },

    /**
     * Adds body to the system at given position
     *
     * @param {Object} pos position of a body
     *
     * @returns {ngraph.physics.primitives.Body} added body
     */
    addBodyAt: function (pos) {
      if (!pos) {
        throw new Error('Body position is required');
      }
      var body = createBody(pos);
      bodies.push(body);

      return body;
    },

    /**
     * Removes body from the system
     *
     * @param {ngraph.physics.primitives.Body} body to remove
     *
     * @returns {Boolean} true if body found and removed. falsy otherwise;
     */
    removeBody: function (body) {
      if (!body) { return; }

      var idx = bodies.indexOf(body);
      if (idx < 0) { return; }

      bodies.splice(idx, 1);
      if (bodies.length === 0) {
        bounds.reset();
      }
      return true;
    },

    /**
     * Adds a spring to this simulation.
     *
     * @returns {Object} - a handle for a spring. If you want to later remove
     * spring pass it to removeSpring() method.
     */
    addSpring: function (body1, body2, springLength, springCoefficient) {
      if (!body1 || !body2) {
        throw new Error('Cannot add null spring to force simulator');
      }

      if (typeof springLength !== 'number') {
        springLength = -1; // assume global configuration
      }

      var spring = new Spring(body1, body2, springLength, springCoefficient >= 0 ? springCoefficient : -1);
      springs.push(spring);

      // TODO: could mark simulator as dirty.
      return spring;
    },

    /**
     * Returns amount of movement performed on last step() call
     */
    getTotalMovement: function () {
      return totalMovement;
    },

    /**
     * Removes spring from the system
     *
     * @param {Object} spring to remove. Spring is an object returned by addSpring
     *
     * @returns {Boolean} true if spring found and removed. falsy otherwise;
     */
    removeSpring: function (spring) {
      if (!spring) { return; }
      var idx = springs.indexOf(spring);
      if (idx > -1) {
        springs.splice(idx, 1);
        return true;
      }
    },

    getBestNewBodyPosition: function (neighbors) {
      return bounds.getBestNewPosition(neighbors);
    },

    /**
     * Returns bounding box which covers all bodies
     */
    getBBox: getBoundingBox, 
    getBoundingBox: getBoundingBox, 

    invalidateBBox: function () {
      console.warn('invalidateBBox() is deprecated, bounds always recomputed on `getBBox()` call');
    },

    // TODO: Move the force specific stuff to force
    gravity: function (value) {
      if (value !== undefined) {
        settings.gravity = value;
        quadTree.options({gravity: value});
        return this;
      } else {
        return settings.gravity;
      }
    },

    theta: function (value) {
      if (value !== undefined) {
        settings.theta = value;
        quadTree.options({theta: value});
        return this;
      } else {
        return settings.theta;
      }
    },

    /**
     * Returns pseudo-random number generator instance.
     */
    random: random
  };

  // allow settings modification via public API:
  expose(settings, publicApi);

  eventify(publicApi);

  return publicApi;

  function getBoundingBox() {
    bounds.update();
    return bounds.box;
  }

  function addForce(forceName, forceFunction) {
    if (forceMap.has(forceName)) throw new Error('Force ' + forceName + ' is already added');

    forceMap.set(forceName, forceFunction);
    forces.push(forceFunction);
  }

  function removeForce(forceName) {
    var forceIndex = forces.indexOf(forceMap.get(forceName));
    if (forceIndex < 0) return;
    forces.splice(forceIndex, 1);
    forceMap.delete(forceName);
  }

  function getForces() {
    // TODO: Should I trust them or clone the forces?
    return forceMap;
  }

  function nbodyForce(/* iterationUmber */) {
    if (bodies.length === 0) return;

    quadTree.insertBodies(bodies);
    var i = bodies.length;
    while (i--) {
      var body = bodies[i];
      if (!body.isPinned) {
        body.reset();
        quadTree.updateBodyForce(body);
        dragForce.update(body);
      }
    }
  }

  function updateSpringForce() {
    var i = springs.length;
    while (i--) {
      springForce.update(springs[i]);
    }
  }

}

function expose(settings, target) {
  for (var key in settings) {
    augment(settings, target, key);
  }
}

function augment(source, target, key) {
  if (!source.hasOwnProperty(key)) return;
  if (typeof target[key] === 'function') {
    // this accessor is already defined. Ignore it
    return;
  }
  var sourceIsNumber = Number.isFinite(source[key]);

  if (sourceIsNumber) {
    target[key] = function (value) {
      if (value !== undefined) {
        if (!Number.isFinite(value)) throw new Error('Value of ' + key + ' should be a valid number.');
        source[key] = value;
        return target;
      }
      return source[key];
    };
  } else {
    target[key] = function (value) {
      if (value !== undefined) {
        source[key] = value;
        return target;
      }
      return source[key];
    };
  }
}

},{"./codeGenerators/generateBounds":3,"./codeGenerators/generateCreateBody":4,"./codeGenerators/generateCreateDragForce":5,"./codeGenerators/generateCreateSpringForce":6,"./codeGenerators/generateIntegrator":7,"./codeGenerators/generateQuadTree":8,"./spring":11,"ngraph.events":12,"ngraph.merge":13,"ngraph.random":14}],11:[function(require,module,exports){
module.exports = Spring;

/**
 * Represents a physical spring. Spring connects two bodies, has rest length
 * stiffness coefficient and optional weight
 */
function Spring(fromBody, toBody, length, springCoefficient) {
    this.from = fromBody;
    this.to = toBody;
    this.length = length;
    this.coefficient = springCoefficient;
}

},{}],12:[function(require,module,exports){
module.exports = function eventify(subject) {
  validateSubject(subject);

  var eventsStorage = createEventsStorage(subject);
  subject.on = eventsStorage.on;
  subject.off = eventsStorage.off;
  subject.fire = eventsStorage.fire;
  return subject;
};

function createEventsStorage(subject) {
  // Store all event listeners to this hash. Key is event name, value is array
  // of callback records.
  //
  // A callback record consists of callback function and its optional context:
  // { 'eventName' => [{callback: function, ctx: object}] }
  var registeredEvents = Object.create(null);

  return {
    on: function (eventName, callback, ctx) {
      if (typeof callback !== 'function') {
        throw new Error('callback is expected to be a function');
      }
      var handlers = registeredEvents[eventName];
      if (!handlers) {
        handlers = registeredEvents[eventName] = [];
      }
      handlers.push({callback: callback, ctx: ctx});

      return subject;
    },

    off: function (eventName, callback) {
      var wantToRemoveAll = (typeof eventName === 'undefined');
      if (wantToRemoveAll) {
        // Killing old events storage should be enough in this case:
        registeredEvents = Object.create(null);
        return subject;
      }

      if (registeredEvents[eventName]) {
        var deleteAllCallbacksForEvent = (typeof callback !== 'function');
        if (deleteAllCallbacksForEvent) {
          delete registeredEvents[eventName];
        } else {
          var callbacks = registeredEvents[eventName];
          for (var i = 0; i < callbacks.length; ++i) {
            if (callbacks[i].callback === callback) {
              callbacks.splice(i, 1);
            }
          }
        }
      }

      return subject;
    },

    fire: function (eventName) {
      var callbacks = registeredEvents[eventName];
      if (!callbacks) {
        return subject;
      }

      var fireArguments;
      if (arguments.length > 1) {
        fireArguments = Array.prototype.splice.call(arguments, 1);
      }
      for(var i = 0; i < callbacks.length; ++i) {
        var callbackInfo = callbacks[i];
        callbackInfo.callback.apply(callbackInfo.ctx, fireArguments);
      }

      return subject;
    }
  };
}

function validateSubject(subject) {
  if (!subject) {
    throw new Error('Eventify cannot use falsy object as events subject');
  }
  var reservedWords = ['on', 'fire', 'off'];
  for (var i = 0; i < reservedWords.length; ++i) {
    if (subject.hasOwnProperty(reservedWords[i])) {
      throw new Error("Subject cannot be eventified, since it already has property '" + reservedWords[i] + "'");
    }
  }
}

},{}],13:[function(require,module,exports){
module.exports = merge;

/**
 * Augments `target` with properties in `options`. Does not override
 * target's properties if they are defined and matches expected type in 
 * options
 *
 * @returns {Object} merged object
 */
function merge(target, options) {
  var key;
  if (!target) { target = {}; }
  if (options) {
    for (key in options) {
      if (options.hasOwnProperty(key)) {
        var targetHasIt = target.hasOwnProperty(key),
            optionsValueType = typeof options[key],
            shouldReplace = !targetHasIt || (typeof target[key] !== optionsValueType);

        if (shouldReplace) {
          target[key] = options[key];
        } else if (optionsValueType === 'object') {
          // go deep, don't care about loops here, we are simple API!:
          target[key] = merge(target[key], options[key]);
        }
      }
    }
  }

  return target;
}

},{}],14:[function(require,module,exports){
module.exports = random;

// TODO: Deprecate?
module.exports.random = random,
module.exports.randomIterator = randomIterator

/**
 * Creates seeded PRNG with two methods:
 *   next() and nextDouble()
 */
function random(inputSeed) {
  var seed = typeof inputSeed === 'number' ? inputSeed : (+new Date());
  return new Generator(seed)
}

function Generator(seed) {
  this.seed = seed;
}

/**
  * Generates random integer number in the range from 0 (inclusive) to maxValue (exclusive)
  *
  * @param maxValue Number REQUIRED. Omitting this number will result in NaN values from PRNG.
  */
Generator.prototype.next = next;

/**
  * Generates random double number in the range from 0 (inclusive) to 1 (exclusive)
  * This function is the same as Math.random() (except that it could be seeded)
  */
Generator.prototype.nextDouble = nextDouble;

/**
 * Returns a random real number uniformly in [0, 1)
 */
Generator.prototype.uniform = nextDouble;

Generator.prototype.gaussian = gaussian;

function gaussian() {
  // use the polar form of the Box-Muller transform
  // based on https://introcs.cs.princeton.edu/java/23recursion/StdRandom.java
  var r, x, y;
  do {
    x = this.nextDouble() * 2 - 1;
    y = this.nextDouble() * 2 - 1;
    r = x * x + y * y;
  } while (r >= 1 || r === 0);

  return x * Math.sqrt(-2 * Math.log(r)/r);
}

function nextDouble() {
  var seed = this.seed;
  // Robert Jenkins' 32 bit integer hash function.
  seed = ((seed + 0x7ed55d16) + (seed << 12)) & 0xffffffff;
  seed = ((seed ^ 0xc761c23c) ^ (seed >>> 19)) & 0xffffffff;
  seed = ((seed + 0x165667b1) + (seed << 5)) & 0xffffffff;
  seed = ((seed + 0xd3a2646c) ^ (seed << 9)) & 0xffffffff;
  seed = ((seed + 0xfd7046c5) + (seed << 3)) & 0xffffffff;
  seed = ((seed ^ 0xb55a4f09) ^ (seed >>> 16)) & 0xffffffff;
  this.seed = seed;
  return (seed & 0xfffffff) / 0x10000000;
}

function next(maxValue) {
  return Math.floor(this.nextDouble() * maxValue);
}

/*
 * Creates iterator over array, which returns items of array in random order
 * Time complexity is guaranteed to be O(n);
 */
function randomIterator(array, customRandom) {
  var localRandom = customRandom || random();
  if (typeof localRandom.next !== 'function') {
    throw new Error('customRandom does not match expected API: next() function is missing');
  }

  return {
    forEach: forEach,

    /**
     * Shuffles array randomly, in place.
     */
    shuffle: shuffle
  };

  function shuffle() {
    var i, j, t;
    for (i = array.length - 1; i > 0; --i) {
      j = localRandom.next(i + 1); // i inclusive
      t = array[j];
      array[j] = array[i];
      array[i] = t;
    }

    return array;
  }

  function forEach(callback) {
    var i, j, t;
    for (i = array.length - 1; i > 0; --i) {
      j = localRandom.next(i + 1); // i inclusive
      t = array[j];
      array[j] = array[i];
      array[i] = t;

      callback(t);
    }

    if (array.length) {
      callback(array[0]);
    }
  }
}
},{}]},{},[1])(1)
});