/*
 * Javascript functions for improve sigma.js functionality
 * Jeff Xia (jeff.xia@mcgill.ca)
 */


//function to highlight node border
sigma.utils.pkg('sigma.canvas.nodes');
sigma.canvas.nodes.def = function (node, context, settings) {
    var prefix = settings('prefix') || '';

    context.fillStyle = node.color || settings('defaultNodeColor');
    context.beginPath();
    context.arc(
            node[prefix + 'x'],
            node[prefix + 'y'],
            node[prefix + 'size'],
            0,
            Math.PI * 2,
            true
            );

    context.closePath();
    context.fill();
    if (node.borderColor) {
        context.lineWidth = 2;
        context.strokeStyle = node.borderColor;
        context.stroke();
    }
};

//function SVG renderer
;
(function (undefined) {
    'use strict';

    if (typeof sigma === 'undefined')
        throw 'sigma is not declared';

    if (typeof conrad === 'undefined')
        throw 'conrad is not declared';

    // Initialize packages:
    sigma.utils.pkg('sigma.renderers');

    /**
     * This function is the constructor of the svg sigma's renderer.
     *
     * @param  {sigma.classes.graph}            graph    The graph to render.
     * @param  {sigma.classes.camera}           camera   The camera.
     * @param  {configurable}           settings The sigma instance settings
     *                                           function.
     * @param  {object}                 object   The options object.
     * @return {sigma.renderers.svg}             The renderer instance.
     */
    sigma.renderers.svg = function (graph, camera, settings, options) {
        if (typeof options !== 'object')
            throw 'sigma.renderers.svg: Wrong arguments.';

        if (!(options.container instanceof HTMLElement))
            throw 'Container not found.';

        var i,
                l,
                a,
                fn,
                self = this;

        sigma.classes.dispatcher.extend(this);

        // Initialize main attributes:
        this.graph = graph;
        this.camera = camera;
        this.domElements = {
            graph: null,
            groups: {},
            nodes: {},
            edges: {},
            labels: {},
            hovers: {}
        };
        this.measurementCanvas = null;
        this.options = options;
        this.container = this.options.container;
        this.settings = (
                typeof options.settings === 'object' &&
                options.settings
                ) ?
                settings.embedObjects(options.settings) :
                settings;

        // Is the renderer meant to be freestyle?
        this.settings('freeStyle', !!this.options.freeStyle);

        // SVG xmlns
        this.settings('xmlns', 'http://www.w3.org/2000/svg');

        // Indexes:
        this.nodesOnScreen = [];
        this.edgesOnScreen = [];

        // Find the prefix:
        this.options.prefix = 'renderer' + sigma.utils.id() + ':';

        // Initialize the DOM elements
        this.initDOM('svg');

        // Initialize captors:
        this.captors = [];
        a = this.options.captors || [sigma.captors.mouse, sigma.captors.touch];
        for (i = 0, l = a.length; i < l; i++) {
            fn = typeof a[i] === 'function' ? a[i] : sigma.captors[a[i]];
            this.captors.push(
                    new fn(
                            this.domElements.graph,
                            this.camera,
                            this.settings
                            )
                    );
        }

        // Bind resize:
        window.addEventListener('resize', function () {
            self.resize();
        });

        // Resize
        this.resize(false);
    };

    /**
     * This method renders the graph on the svg scene.
     *
     * @param  {?object}                options Eventually an object of options.
     * @return {sigma.renderers.svg}            Returns the instance itself.
     */
    sigma.renderers.svg.prototype.render = function (options) {
        options = options || {};

        var a,
                i,
                k,
                e,
                l,
                o,
                source,
                target,
                start,
                edges,
                renderers,
                subrenderers,
                index = {},
                graph = this.graph,
                nodes = this.graph.nodes,
                prefix = this.options.prefix || '',
                drawEdges = this.settings(options, 'drawEdges'),
                drawNodes = this.settings(options, 'drawNodes'),
                drawLabels = this.settings(options, 'drawLabels'),
                embedSettings = this.settings.embedObjects(options, {
                    prefix: this.options.prefix,
                    forceLabels: this.options.forceLabels
                });

        // Check the 'hideEdgesOnMove' setting:
        if (this.settings(options, 'hideEdgesOnMove'))
            if (this.camera.isAnimated || this.camera.isMoving)
                drawEdges = false;

        // Apply the camera's view:
        this.camera.applyView(
                undefined,
                this.options.prefix,
                {
                    width: this.width,
                    height: this.height
                }
        );

        // Hiding everything
        // TODO: find a more sensible way to perform this operation
        this.hideDOMElements(this.domElements.nodes);
        this.hideDOMElements(this.domElements.edges);
        this.hideDOMElements(this.domElements.labels);

        // Find which nodes are on screen
        this.edgesOnScreen = [];
        this.nodesOnScreen = this.camera.quadtree.area(
                this.camera.getRectangle(this.width, this.height)
                );

        // Node index
        for (a = this.nodesOnScreen, i = 0, l = a.length; i < l; i++)
            index[a[i].id] = a[i];

        // Find which edges are on screen
        for (a = graph.edges(), i = 0, l = a.length; i < l; i++) {
            o = a[i];
            if (
                    (index[o.source] || index[o.target]) &&
                    (!o.hidden && !nodes(o.source).hidden && !nodes(o.target).hidden)
                    )
                this.edgesOnScreen.push(o);
        }

        // Display nodes
        //---------------
        renderers = sigma.svg.nodes;
        subrenderers = sigma.svg.labels;

        //-- First we create the nodes which are not already created
        if (drawNodes)
            for (a = this.nodesOnScreen, i = 0, l = a.length; i < l; i++) {
                if (!a[i].hidden && !this.domElements.nodes[a[i].id]) {

                    // Node
                    e = (renderers[a[i].type] || renderers.def).create(
                            a[i],
                            embedSettings
                            );

                    this.domElements.nodes[a[i].id] = e;
                    this.domElements.groups.nodes.appendChild(e);

                    // Label
                    e = (subrenderers[a[i].type] || subrenderers.def).create(
                            a[i],
                            embedSettings
                            );

                    this.domElements.labels[a[i].id] = e;
                    this.domElements.groups.labels.appendChild(e);
                }
            }

        //-- Second we update the nodes
        if (drawNodes)
            for (a = this.nodesOnScreen, i = 0, l = a.length; i < l; i++) {

                if (a[i].hidden)
                    continue;

                // Node
                (renderers[a[i].type] || renderers.def).update(
                        a[i],
                        this.domElements.nodes[a[i].id],
                        embedSettings
                        );

                // Label
                (subrenderers[a[i].type] || subrenderers.def).update(
                        a[i],
                        this.domElements.labels[a[i].id],
                        embedSettings
                        );
            }

        // Display edges
        //---------------
        renderers = sigma.svg.edges;

        //-- First we create the edges which are not already created
        if (drawEdges)
            for (a = this.edgesOnScreen, i = 0, l = a.length; i < l; i++) {
                if (!this.domElements.edges[a[i].id]) {
                    source = nodes(a[i].source);
                    target = nodes(a[i].target);

                    e = (renderers[a[i].type] || renderers.def).create(
                            a[i],
                            source,
                            target,
                            embedSettings
                            );

                    this.domElements.edges[a[i].id] = e;
                    this.domElements.groups.edges.appendChild(e);
                }
            }

        //-- Second we update the edges
        if (drawEdges)
            for (a = this.edgesOnScreen, i = 0, l = a.length; i < l; i++) {
                source = nodes(a[i].source);
                target = nodes(a[i].target);

                (renderers[a[i].type] || renderers.def).update(
                        a[i],
                        this.domElements.edges[a[i].id],
                        source,
                        target,
                        embedSettings
                        );
            }

        this.dispatchEvent('render');

        return this;
    };

    /**
     * This method creates a DOM element of the specified type, switches its
     * position to "absolute", references it to the domElements attribute, and
     * finally appends it to the container.
     *
     * @param  {string} tag The label tag.
     * @param  {string} id  The id of the element (to store it in "domElements").
     */
    sigma.renderers.svg.prototype.initDOM = function (tag) {
        var dom = document.createElementNS(this.settings('xmlns'), tag),
                c = this.settings('classPrefix'),
                g,
                l,
                i;

        dom.style.position = 'absolute';
        dom.setAttribute('class', c + '-svg');

        // Setting SVG namespace
        dom.setAttribute('xmlns', this.settings('xmlns'));
        dom.setAttribute('xmlns:xlink', 'http://www.w3.org/1999/xlink');
        dom.setAttribute('version', '1.1');

        // Creating the measurement canvas
        var canvas = document.createElement('canvas');
        canvas.setAttribute('class', c + '-measurement-canvas');

        // Appending elements
        this.domElements.graph = this.container.appendChild(dom);

        // Creating groups
        var groups = ['edges', 'nodes', 'labels', 'hovers'];
        for (i = 0, l = groups.length; i < l; i++) {
            g = document.createElementNS(this.settings('xmlns'), 'g');

            g.setAttributeNS(null, 'id', c + '-group-' + groups[i]);
            g.setAttributeNS(null, 'class', c + '-group');

            this.domElements.groups[groups[i]] =
                    this.domElements.graph.appendChild(g);
        }

        // Appending measurement canvas
        this.container.appendChild(canvas);
        this.measurementCanvas = canvas.getContext('2d');
    };

    /**
     * This method hides a batch of SVG DOM elements.
     *
     * @param  {array}                  elements  An array of elements to hide.
     * @param  {object}                 renderer  The renderer to use.
     * @return {sigma.renderers.svg}              Returns the instance itself.
     */
    sigma.renderers.svg.prototype.hideDOMElements = function (elements) {
        var o,
                i;

        for (i in elements) {
            o = elements[i];
            sigma.svg.utils.hide(o);
        }

        return this;
    };

    /**
     * This method binds the hover events to the renderer.
     *
     * @param  {string} prefix The renderer prefix.
     */
    // TODO: add option about whether to display hovers or not
    sigma.renderers.svg.prototype.bindHovers = function (prefix) {
        var renderers = sigma.svg.hovers,
                self = this,
                hoveredNode;

        function overNode(e) {
            var node = e.data.node,
                    embedSettings = self.settings.embedObjects({
                        prefix: prefix
                    });

            if (!embedSettings('enableHovering'))
                return;

            var hover = (renderers[node.type] || renderers.def).create(
                    node,
                    self.domElements.nodes[node.id],
                    self.measurementCanvas,
                    embedSettings
                    );

            self.domElements.hovers[node.id] = hover;

            // Inserting the hover in the dom
            self.domElements.groups.hovers.appendChild(hover);
            hoveredNode = node;
        }

        function outNode(e) {
            var node = e.data.node,
                    embedSettings = self.settings.embedObjects({
                        prefix: prefix
                    });

            if (!embedSettings('enableHovering'))
                return;

            // Deleting element
            self.domElements.groups.hovers.removeChild(
                    self.domElements.hovers[node.id]
                    );
            hoveredNode = null;
            delete self.domElements.hovers[node.id];

            // Reinstate
            self.domElements.groups.nodes.appendChild(
                    self.domElements.nodes[node.id]
                    );
        }

        // OPTIMIZE: perform a real update rather than a deletion
        function update() {
            if (!hoveredNode)
                return;

            var embedSettings = self.settings.embedObjects({
                prefix: prefix
            });

            // Deleting element before update
            self.domElements.groups.hovers.removeChild(
                    self.domElements.hovers[hoveredNode.id]
                    );
            delete self.domElements.hovers[hoveredNode.id];

            var hover = (renderers[hoveredNode.type] || renderers.def).create(
                    hoveredNode,
                    self.domElements.nodes[hoveredNode.id],
                    self.measurementCanvas,
                    embedSettings
                    );

            self.domElements.hovers[hoveredNode.id] = hover;

            // Inserting the hover in the dom
            self.domElements.groups.hovers.appendChild(hover);
        }

        // Binding events
        this.bind('overNode', overNode);
        this.bind('outNode', outNode);

        // Update on render
        this.bind('render', update);
    };

    /**
     * This method resizes each DOM elements in the container and stores the new
     * dimensions. Then, it renders the graph.
     *
     * @param  {?number}                width  The new width of the container.
     * @param  {?number}                height The new height of the container.
     * @return {sigma.renderers.svg}           Returns the instance itself.
     */
    sigma.renderers.svg.prototype.resize = function (w, h) {
        var oldWidth = this.width,
                oldHeight = this.height,
                pixelRatio = 1;

        if (w !== undefined && h !== undefined) {
            this.width = w;
            this.height = h;
        } else {
            this.width = this.container.offsetWidth;
            this.height = this.container.offsetHeight;

            w = this.width;
            h = this.height;
        }

        if (oldWidth !== this.width || oldHeight !== this.height) {
            this.domElements.graph.style.width = w + 'px';
            this.domElements.graph.style.height = h + 'px';

            if (this.domElements.graph.tagName.toLowerCase() === 'svg') {
                this.domElements.graph.setAttribute('width', (w * pixelRatio));
                this.domElements.graph.setAttribute('height', (h * pixelRatio));
            }
        }

        return this;
    };


    /**
     * The labels, nodes and edges renderers are stored in the three following
     * objects. When an element is drawn, its type will be checked and if a
     * renderer with the same name exists, it will be used. If not found, the
     * default renderer will be used instead.
     *
     * They are stored in different files, in the "./svg" folder.
     */
    sigma.utils.pkg('sigma.svg.nodes');
    sigma.utils.pkg('sigma.svg.edges');
    sigma.utils.pkg('sigma.svg.labels');
}).call(this);


//functions to export as SVG
(function (undefined) {
    'use strict';

    /**
     * Sigma SVG Exporter
     * ===================
     *
     * This plugin is designed to export a graph to a svg file that can be
     * downloaded or just used elsewhere.
     *
     * Author: Guillaume Plique (Yomguithereal)
     * Version: 0.0.1
     */

    // Terminating if sigma were not to be found
    if (typeof sigma === 'undefined')
        throw 'sigma.renderers.snapshot: sigma not in scope.';


    /**
     * Polyfills
     */
    var URL = this.URL || this.webkitURL || this;


    /**
     * Utilities
     */
    function createBlob(data) {
        return new Blob(
                [data],
                {type: 'image/svg+xml;charset=utf-8'}
        );
    }

    function download(string, filename) {

        // Creating blob href
        var blob = createBlob(string);

        // Anchor
        var o = {};
        o.anchor = document.createElement('a');
        o.anchor.setAttribute('href', URL.createObjectURL(blob));
        o.anchor.setAttribute('download', filename);

        // Click event
        var event = document.createEvent('MouseEvent');
        event.initMouseEvent('click', true, false, window, 0, 0, 0, 0, 0,
                false, false, false, false, 0, null);

        URL.revokeObjectURL(blob);

        o.anchor.dispatchEvent(event);
        delete o.anchor;
    }


    /**
     * Defaults
     */
    var DEFAULTS = {
        size: '1000',
        width: '1000',
        height: '1000',
        classes: true,
        labels: true,
        data: false,
        download: false,
        filename: 'graph.svg'
    };

    var XMLNS = 'http://www.w3.org/2000/svg';


    /**
     * Subprocesses
     */
    function optimize(svg, prefix, params) {
        var nodeColorIndex = {},
                edgeColorIndex = {},
                count = 0,
                color,
                style,
                styleText = '',
                f,
                i,
                l;

        // Creating style tag if needed
        if (params.classes) {
            style = document.createElementNS(XMLNS, 'style');
            svg.insertBefore(style, svg.firstChild);
        }

        // Iterating over nodes
        var nodes = svg.querySelectorAll('[id="' + prefix + '-group-nodes"] > [class="' + prefix + '-node"]');

        for (i = 0, l = nodes.length, f = true; i < l; i++) {
            color = nodes[i].getAttribute('fill');

            if (!params.data)
                nodes[i].removeAttribute('data-node-id');

            if (params.classes) {

                if (!(color in nodeColorIndex)) {
                    nodeColorIndex[color] = (f ? prefix + '-node' : 'c-' + (count++));
                    styleText += '.' + nodeColorIndex[color] + '{fill: ' + color + '}';
                }

                if (nodeColorIndex[color] !== prefix + '-node')
                    nodes[i].setAttribute('class', nodes[i].getAttribute('class') + ' ' + nodeColorIndex[color]);
                nodes[i].removeAttribute('fill');
            }

            f = false;
        }

        // Iterating over edges
        var edges = svg.querySelectorAll('[id="' + prefix + '-group-edges"] > [class="' + prefix + '-edge"]');

        for (i = 0, l = edges.length, f = true; i < l; i++) {
            color = edges[i].getAttribute('stroke');

            if (!params.data)
                edges[i].removeAttribute('data-edge-id');

            if (params.classes) {

                if (!(color in edgeColorIndex)) {
                    edgeColorIndex[color] = (f ? prefix + '-edge' : 'c-' + (count++));
                    styleText += '.' + edgeColorIndex[color] + '{stroke: ' + color + '}';
                }

                if (edgeColorIndex[color] !== prefix + '-edge')
                    edges[i].setAttribute('class', edges[i].getAttribute('class') + ' ' + edgeColorIndex[color]);
                edges[i].removeAttribute('stroke');
            }

            f = false;
        }

        if (params.classes)
            style.appendChild(document.createTextNode(styleText));
    }


    /**
     * Extending prototype
     */
    sigma.prototype.toSVG = function (params) {
        params = params || {};

        var prefix = this.settings('classPrefix'),
                w = params.size || params.width || DEFAULTS.size,
                h = params.size || params.height || DEFAULTS.size;
        var bgcolor = params.background;
        // Creating a dummy container
        var container = document.createElement('div');
        container.setAttribute('width', w);
        container.setAttribute('height', h);
        container.setAttribute('style', 'position:absolute; top: 0px; left:0px; width: ' + w + 'px; height: ' + h + 'px;');

        // Creating a camera
        var camera = this.addCamera();

        // Creating a svg renderer
        var renderer = this.addRenderer({
            camera: camera,
            container: container,
            type: 'svg',
            forceLabels: !!params.labels
        });

        // Refreshing
        renderer.resize(w, h);
        this.refresh();

        // Dropping camera and renderers before something nasty happens
        this.killRenderer(renderer);
        this.killCamera(camera);

        // Retrieving svg
        var svg = container.querySelector('svg');
        svg.removeAttribute('style');
        svg.setAttribute('width', w + 'px');
        svg.setAttribute('height', h + 'px');
        svg.setAttribute('x', '0px');
        svg.setAttribute('y', '0px');
        svg.setAttribute("style", "overflow:hidden; background-color:" + bgcolor);
        // svg.setAttribute('viewBox', '0 0 1000 1000');

        // Dropping labels
        if (!params.labels) {
            var labelGroup = svg.querySelector('[id="' + prefix + '-group-labels"]');
            svg.removeChild(labelGroup);
        }

        // update labels (only label those nodes with size larger than the threshold
        var nodes = svg.querySelectorAll('[id="' + prefix + '-group-nodes"] > [class="' + prefix + '-node"]');
        var labels = svg.querySelectorAll('[id="' + prefix + '-group-labels"] > [class="' + prefix + '-label"]');

        var i, nd, size;
        var thresh = this.settings('labelThreshold');
        for (i = 0; i < nodes.length; i++) {
            nd = nodes[i];
            size = nd.getAttribute('r');
            if (size < thresh) {
                labels[i].textContent = '';
            }
        }

        // Dropping hovers
        var hoverGroup = svg.querySelector('[id="' + prefix + '-group-hovers"]');
        svg.removeChild(hoverGroup);

        // Optims?
        params.classes = (params.classes !== false);
        if (!params.data || params.classes)
            optimize(svg, prefix, params);

        // Retrieving svg string
        var svgString = svg.outerHTML;

        // Paranoid cleanup
        container = null;

        // Output string
        var output = '<?xml version="1.0" encoding="utf-8"?>\n';
        output += '<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">\n';
        output += svgString;

        if (params.download)
            download(output, params.filename || DEFAULTS.filename);

        return output;
    };
}).call(this);

//function svg edge support
// edges def
;
(function () {
    'use strict';

    sigma.utils.pkg('sigma.svg.edges');

    /**
     * The default edge renderer. It renders the node as a simple line.
     */
    sigma.svg.edges.def = {
        /**
         * SVG Element creation.
         *
         * @param  {object}                   edge       The edge object.
         * @param  {object}                   source     The source node object.
         * @param  {object}                   target     The target node object.
         * @param  {configurable}             settings   The settings function.
         */
        create: function (edge, source, target, settings) {
            var color = edge.color,
                    prefix = settings('prefix') || '',
                    edgeColor = settings('edgeColor'),
                    defaultNodeColor = settings('defaultNodeColor'),
                    defaultEdgeColor = settings('defaultEdgeColor');

            if (!color)
                switch (edgeColor) {
                    case 'source':
                        color = source.color || defaultNodeColor;
                        break;
                    case 'target':
                        color = target.color || defaultNodeColor;
                        break;
                    default:
                        color = defaultEdgeColor;
                        break;
                }

            var line = document.createElementNS(settings('xmlns'), 'line');

            // Attributes
            line.setAttributeNS(null, 'data-edge-id', edge.id);
            line.setAttributeNS(null, 'class', settings('classPrefix') + '-edge');
            line.setAttributeNS(null, 'stroke', color);

            return line;
        },
        /**
         * SVG Element update.
         *
         * @param  {object}                   edge       The edge object.
         * @param  {DOMElement}               line       The line DOM Element.
         * @param  {object}                   source     The source node object.
         * @param  {object}                   target     The target node object.
         * @param  {configurable}             settings   The settings function.
         */
        update: function (edge, line, source, target, settings) {
            var prefix = settings('prefix') || '';

            line.setAttributeNS(null, 'stroke-width', edge[prefix + 'size'] || 1);
            line.setAttributeNS(null, 'x1', source[prefix + 'x']);
            line.setAttributeNS(null, 'y1', source[prefix + 'y']);
            line.setAttributeNS(null, 'x2', target[prefix + 'x']);
            line.setAttributeNS(null, 'y2', target[prefix + 'y']);

            // Showing
            line.style.display = '';

            return this;
        }
    };
})();

//edges curve
;
(function () {
    'use strict';

    sigma.utils.pkg('sigma.svg.edges');

    /**
     * The curve edge renderer. It renders the node as a bezier curve.
     */
    sigma.svg.edges.curve = {
        /**
         * SVG Element creation.
         *
         * @param  {object}                   edge       The edge object.
         * @param  {object}                   source     The source node object.
         * @param  {object}                   target     The target node object.
         * @param  {configurable}             settings   The settings function.
         */
        create: function (edge, source, target, settings) {
            var color = edge.color,
                    prefix = settings('prefix') || '',
                    edgeColor = settings('edgeColor'),
                    defaultNodeColor = settings('defaultNodeColor'),
                    defaultEdgeColor = settings('defaultEdgeColor');

            if (!color)
                switch (edgeColor) {
                    case 'source':
                        color = source.color || defaultNodeColor;
                        break;
                    case 'target':
                        color = target.color || defaultNodeColor;
                        break;
                    default:
                        color = defaultEdgeColor;
                        break;
                }

            var path = document.createElementNS(settings('xmlns'), 'path');

            // Attributes
            path.setAttributeNS(null, 'data-edge-id', edge.id);
            path.setAttributeNS(null, 'class', settings('classPrefix') + '-edge');
            path.setAttributeNS(null, 'stroke', color);

            return path;
        },
        /**
         * SVG Element update.
         *
         * @param  {object}                   edge       The edge object.
         * @param  {DOMElement}               line       The line DOM Element.
         * @param  {object}                   source     The source node object.
         * @param  {object}                   target     The target node object.
         * @param  {configurable}             settings   The settings function.
         */
        update: function (edge, path, source, target, settings) {
            var prefix = settings('prefix') || '';

            path.setAttributeNS(null, 'stroke-width', edge[prefix + 'size'] || 1);

            // Control point
            var cx = (source[prefix + 'x'] + target[prefix + 'x']) / 2 +
                    (target[prefix + 'y'] - source[prefix + 'y']) / 4,
                    cy = (source[prefix + 'y'] + target[prefix + 'y']) / 2 +
                    (source[prefix + 'x'] - target[prefix + 'x']) / 4;

            // Path
            var p = 'M' + source[prefix + 'x'] + ',' + source[prefix + 'y'] + ' ' +
                    'Q' + cx + ',' + cy + ' ' +
                    target[prefix + 'x'] + ',' + target[prefix + 'y'];

            // Updating attributes
            path.setAttributeNS(null, 'd', p);
            path.setAttributeNS(null, 'fill', 'none');

            // Showing
            path.style.display = '';

            return this;
        }
    };
})();

//svg node def
;
(function () {
    'use strict';

    sigma.utils.pkg('sigma.svg.nodes');

    /**
     * The default node renderer. It renders the node as a simple disc.
     */
    sigma.svg.nodes.def = {
        /**
         * SVG Element creation.
         *
         * @param  {object}                   node     The node object.
         * @param  {configurable}             settings The settings function.
         */
        create: function (node, settings) {
            var prefix = settings('prefix') || '',
                    circle = document.createElementNS(settings('xmlns'), 'circle');

            // Defining the node's circle
            circle.setAttributeNS(null, 'data-node-id', node.id);
            circle.setAttributeNS(null, 'class', settings('classPrefix') + '-node');
            circle.setAttributeNS(
                    null, 'fill', node.color || settings('defaultNodeColor'));

            // Returning the DOM Element
            return circle;
        },
        /**
         * SVG Element update.
         *
         * @param  {object}                   node     The node object.
         * @param  {DOMElement}               circle   The node DOM element.
         * @param  {configurable}             settings The settings function.
         */
        update: function (node, circle, settings) {
            var prefix = settings('prefix') || '';

            // Applying changes
            // TODO: optimize - check if necessary
            circle.setAttributeNS(null, 'cx', node[prefix + 'x']);
            circle.setAttributeNS(null, 'cy', node[prefix + 'y']);
            circle.setAttributeNS(null, 'r', node[prefix + 'size']);

            // Updating only if not freestyle
            if (!settings('freeStyle'))
                circle.setAttributeNS(
                        null, 'fill', node.color || settings('defaultNodeColor'));

            // Showing
            circle.style.display = '';

            return this;
        }
    };
})();

//svg labels
;
(function (undefined) {
    'use strict';

    if (typeof sigma === 'undefined')
        throw 'sigma is not declared';

    // Initialize packages:
    sigma.utils.pkg('sigma.svg.labels');

    /**
     * The default label renderer. It renders the label as a simple text.
     */
    sigma.svg.labels.def = {
        /**
         * SVG Element creation.
         *
         * @param  {object}                   node       The node object.
         * @param  {configurable}             settings   The settings function.
         */
        create: function (node, settings) {
            var prefix = settings('prefix') || '',
                    size = node[prefix + 'size'],
                    text = document.createElementNS(settings('xmlns'), 'text');

            var fontSize = (settings('labelSize') === 'fixed') ?
                    settings('defaultLabelSize') :
                    settings('labelSizeRatio') * size;

            var fontColor = (settings('labelColor') === 'node') ?
                    (node.color || settings('defaultNodeColor')) :
                    settings('defaultLabelColor');

            text.setAttributeNS(null, 'data-label-target', node.id);
            text.setAttributeNS(null, 'class', settings('classPrefix') + '-label');
            text.setAttributeNS(null, 'font-size', fontSize);
            text.setAttributeNS(null, 'font-family', settings('font'));
            text.setAttributeNS(null, 'fill', fontColor);

            text.innerHTML = node.label;
            text.textContent = node.label;

            return text;
        },
        /**
         * SVG Element update.
         *
         * @param  {object}                   node     The node object.
         * @param  {DOMElement}               text     The label DOM element.
         * @param  {configurable}             settings The settings function.
         */
        update: function (node, text, settings) {
            var prefix = settings('prefix') || '',
                    size = node[prefix + 'size'];

            var fontSize = (settings('labelSize') === 'fixed') ?
                    settings('defaultLabelSize') :
                    settings('labelSizeRatio') * size;

            // Case when we don't want to display the label
            if (!settings('forceLabels') && size < settings('labelThreshold'))
                return;

            if (typeof node.label !== 'string')
                return;

            // Updating
            text.setAttributeNS(null, 'x',
                    Math.round(node[prefix + 'x'] + size + 3));
            text.setAttributeNS(null, 'y',
                    Math.round(node[prefix + 'y'] + fontSize / 3));

            // Showing
            text.style.display = '';

            return this;
        }
    };
}).call(this);
