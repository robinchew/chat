function importSeviewMithril(m) {
  const processAttrs = (attrs = {}) => {
    return attrs;
  };
  return (node) =>
    (typeof node === 'string')
      ? { tag: '#', children: node }
      : node.attrs && node.attrs.innerHTML
        ? m(node.tag, m.trust(node.attrs.innerHTML))
        : m(node.tag, processAttrs(node.attrs), node.children || []);
}
