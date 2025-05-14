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
            var prefix = settings('prefix') || '';
            var shape;
            if (node.type === "circle") {
                shape = document.createElementNS(settings('xmlns'), 'circle');
            } else {
                shape = document.createElementNS(settings('xmlns'), 'rect');
            }
            // Defining the node's circle
            shape.setAttributeNS(null, 'data-node-id', node.id);
            shape.setAttributeNS(null, 'class', settings('classPrefix') + '-node');
            shape.setAttributeNS(
                    null, 'fill', node.color || settings('defaultNodeColor'));

            // Returning the DOM Element
            return shape;
        },
        /**
         * SVG Element update.
         *
         * @param  {object}                   node     The node object.
         * @param  {DOMElement}               circle   The node DOM element.
         * @param  {configurable}             settings The settings function.
         */
        update: function (node, shape, settings) {
            var prefix = settings('prefix') || '';
            var size; var rotate = Math.PI*45/180;
            // Applying changes
            // TODO: optimize - check if necessary
            if (node.type === "circle") {
                shape.setAttributeNS(null, 'cx', node[prefix + 'x']);
                shape.setAttributeNS(null, 'cy', node[prefix + 'y']);
                shape.setAttributeNS(null, 'r', node[prefix + 'size']);
                shape.setAttributeNS(null, 'type', 'target');
            } else {
                size = 2*node[prefix + 'size'];
                shape.setAttributeNS(null, 'x', node[prefix + 'x']-size*Math.sin(rotate));
                shape.setAttributeNS(null, 'y', node[prefix + 'y']-size*Math.sin(rotate));
                shape.setAttributeNS(null, 'width', size);
                shape.setAttributeNS(null, 'height', size);
                shape.setAttributeNS(null, 'type', 'mir');
            }
            // Updating only if not freestyle
            if (!settings('freeStyle'))
                shape.setAttributeNS(
                        null, 'fill', node.color || settings('defaultNodeColor'));

            // Showing
            shape.style.display = '';

            return this;
        }
    };
})();
