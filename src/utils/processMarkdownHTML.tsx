// file not used at the moment

// @ts-ignore
import Replacements from "@content/HTMLReplacements.yml";

interface ReplacementProp {
  toReplace: string;
  with: string;
}

const processHTML = (html: string) => {
  let newHTML: string = html;
  Replacements.forEach((element: ReplacementProp) => {
    newHTML = newHTML.replace(element.toReplace, element.with);
  });
  return newHTML;
};

export { processHTML as processMarkdownHTML };
