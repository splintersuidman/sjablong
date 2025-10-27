export const filter = (ctx) => () => ctx.filter;
export const setFilter = (ctx) => (filter) => () => ctx.filter = filter;
