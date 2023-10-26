function importRoutes({
  patchinko: O,
  ramda: {
    curry,
    defaultTo,
    identity,
    map,
  },
  window,
}) {
  function format(template, params) {
    return template.replace(/:([^/]+)/g, (whole, group1) => params[group1]);
  }
  function generatePath(routes, urlName, params) {
    const [pathTemplate] = routes.find(([, name,]) => name === urlName);
    return format(pathTemplate, params);
  }
  function find(l, match) {
    for (item of l) {
      const matched = match(item);
      if (matched) {
        return matched;
      }
    }
  }
  const pathToRoute = (routes) => (path) => {
    return find(routes, (item) => {
      const [pathTemplate] = item;
      const regexTemplate = '^' + pathTemplate.replace(/:([^/]+)/g, '(?<$1>[^/]+)') + '$';
      const found = new RegExp(regexTemplate).exec(path);
      if (found === null) {
        return null;
      }
      return [item, found.groups];
    });
  }
  const changeView = (routes, hashRouteMode) => (urlName, params = {}) => {
    const encodedParams = map(encodeURIComponent, params || {});
    const path = generatePath(routes, urlName, encodedParams);
    return st => {
      window.history.pushState(
        {},
        '',
        (hashRouteMode ? '#' : '') + path);
      return O(st, {
        route: {
          name: urlName,
          params,
        },
      });
    };
  };
  return {
    init(routes, { hash }) {
      const findRoute = pathToRoute(routes);
      return {
        changeView: changeView(routes, hash),
        getCurrentRoute: () => {
          const path = hash ? window.location.hash.slice(1) : window.location.pathname;
          return findRoute(path);
        },
      };
    },
  }
}
