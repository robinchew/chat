(function (global, factory) {
  typeof exports === 'object' && typeof module !== 'undefined' ? factory(exports) :
  typeof define === 'function' && define.amd ? define(['exports'], factory) :
  (global = typeof globalThis !== 'undefined' ? globalThis : global || self, factory(global.seview = {}));
})(this, (function (exports) { 'use strict';

  function _typeof(obj) {
    "@babel/helpers - typeof";

    return _typeof = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function (obj) {
      return typeof obj;
    } : function (obj) {
      return obj && "function" == typeof Symbol && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj;
    }, _typeof(obj);
  }

  var isString = function isString(x) {
    return typeof x === 'string';
  };
  var isNumber = function isNumber(x) {
    return typeof x === 'number';
  };
  var isBoolean = function isBoolean(x) {
    return typeof x === 'boolean';
  };
  var isArray = function isArray(x) {
    return Array.isArray(x);
  };
  var isIterable = function isIterable(x) {
    return x != null && typeof x[Symbol.iterator] === 'function' && !isString(x);
  };
  var isObject = function isObject(x) {
    return x != null && _typeof(x) === 'object' && !isArray(x) && !isIterable(x);
  };
  var getString = function getString(value) {
    var result = undefined;
    if (isString(value) && value.length > 0) {
      result = value;
    } else if (isNumber(value)) {
      result = String(value);
    } else if (isBoolean(value) && value) {
      result = String(value);
    }
    return result;
  };
  var get = function get(object, path) {
    return object == null ? undefined : path.length === 1 ? object[path[0]] : get(object[path[0]], path.slice(1));
  };
  var set = function set(object, path, value) {
    if (path.length === 1) {
      if (isObject(object[path[0]])) {
        Object.assign(object[path[0]], value);
      } else {
        object[path[0]] = value;
      }
    } else {
      if (object[[path[0]]] == null) {
        object[[path[0]]] = {};
      }
      set(object[path[0]], path.slice(1), value);
    }
    return object;
  };

  // Credit: JSnoX https://github.com/af/JSnoX/blob/master/jsnox.js

  // matches 'input', 'input:text'
  var tagTypeRegex = /^([A-Za-z0-9-]+)(?::([a-z]+))?/;

  // matches '#id', '.class', '[name=value]', '[required]'
  var propsRegex = /((?:#|\.|@)[\w-]+)|(\[.*?\])/g;

  // matches '[name=value]' or '[required]'
  var attrRegex = /\[([\w-]+)(?:=([^\]]+))?\]/;

  /*
  returns tag properties: for example, 'input:password#duck.quack.yellow[name=pwd][required]'
  {
    tag: 'input',
    class: 'quack yellow',
    attrs: { type: 'password', id: 'duck', name: 'pwd', required: true }
  }
  */
  var getTagProperties = function getTagProperties(selector) {
    var result = {};
    var tagType = selector.match(tagTypeRegex);

    // Use div by default
    if (!tagType) {
      tagType = ['div', 'div'];
    }
    result.tag = tagType[1];
    if (tagType[2]) {
      result.attrs = {
        type: tagType[2]
      };
    }
    var tagProps = selector.match(propsRegex);
    if (tagProps) {
      var classes = [];
      tagProps.forEach(function (tagProp) {
        var ch = tagProp[0];
        var prop = tagProp.slice(1);
        if (ch === '#') {
          set(result, ['attrs', 'id'], prop);
        } else if (ch === '.') {
          classes.push(prop);
        } else if (ch === '[') {
          var attrs = tagProp.match(attrRegex);
          set(result, ['attrs', attrs[1]], attrs[2] || true);
        }
      });
      if (classes.length > 0) {
        set(result, ['attrs', 'class'], classes.join(' '));
      }
    }
    return result;
  };

  /*
  returns node definition, expanding on the above tag properties and adding to obtain:
  {
    tag: 'input',
    class: 'quack yellow',
    attrs: { type: 'password', id: 'duck', name: 'pwd', required: true, onClick: ... },
    children: [ { tag: ... }, 'text', ... ]
  }
  */
  var processChildren = function processChildren(rest) {
    var result = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : [];
    rest.forEach(function (child) {
      if (isIterable(child)) {
        child = Array.from(child);
      }
      // Text node
      if (getString(child)) {
        result.push(getString(child));
      } else if (isArray(child)) {
        // Nested array
        if (isArray(child[0]) || isIterable(child[0])) {
          processChildren(child, result);
        }
        // Regular node
        else if (child.length > 0) {
          result.push(nodeDef(child));
        }
      }
    });
    return result;
  };
  var nodeDef = function nodeDef(node) {
    // Tag
    var rest = node[2];
    var varArgsLimit = 3;

    // Process tag
    var result = isString(node[0]) ? getTagProperties(node[0]) : {
      tag: node[0]
    };

    // Process attrs
    if (isObject(node[1])) {
      var attrs = node[1];

      // Process class
      if (attrs['class'] !== undefined) {
        var classAttr = attrs['class'];
        delete attrs['class'];
        var addClasses = [];
        if (isString(classAttr)) {
          addClasses = classAttr.split(' ');
        } else if (isObject(classAttr)) {
          Object.keys(classAttr).forEach(function (key) {
            if (classAttr[key]) {
              addClasses.push(key);
            }
          });
        }
        if (addClasses.length > 0) {
          var existingClass = get(result, ['attrs', 'class']);
          var addClass = addClasses.join(' ');
          set(result, ['attrs', 'class'], (existingClass ? existingClass + ' ' : '') + addClass);
        }
      }

      // Add remaining attributes
      if (Object.keys(attrs).length > 0) {
        if (result.attrs === undefined) {
          result.attrs = attrs;
        } else {
          result.attrs = Object.assign(result.attrs, attrs);
        }
      }
    }
    // No attrs, use second argument as rest
    else {
      rest = node[1];
      varArgsLimit = 2;
    }

    // Process children: varargs
    if (node.length > varArgsLimit) {
      result.children = processChildren(node.slice(varArgsLimit - 1));
    }
    // Process children: one child arg
    else {
      // Text node
      if (getString(rest)) {
        result.children = [getString(rest)];
      }
      if (isIterable(rest)) {
        rest = Array.from(rest);
      }
      if (isArray(rest)) {
        // Array of children vs One child node
        result.children = processChildren(isArray(rest[0]) ? rest : [rest]);
      }
    }
    return result;
  };

  var transformNodeDef = function transformNodeDef(transform, def) {
    if (isArray(def.children) || isIterable(def.children)) {
      var result = [];
      def.children.forEach(function (child) {
        result.push(isString(child) ? transform(child) : transformNodeDef(transform, child));
      });
      def.children = result;
    }
    return transform(def);
  };
  var seview = function seview(transform, options) {
    return function (node) {
      var def = nodeDef(node);
      return transformNodeDef(transform, def);
    };
  };

  exports.seview = seview;

}));
