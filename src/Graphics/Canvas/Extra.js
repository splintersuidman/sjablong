export const canvasImageSourceWidth = (img) => img.width;
export const canvasImageSourceHeight = (img) => img.height;

export const letterSpacing = (ctx) => () => ctx.letterSpacing;
export const setLetterSpacing = (ctx) => (letterSpacing) => () => ctx.letterSpacing = letterSpacing;
