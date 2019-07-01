const { pathsToModuleNameMapper } = require("ts-jest/utils")
const { compilerOptions } = require("../tsconfig")

module.exports = {
  transform: {
    "^.+\\.tsx?$": "ts-jest",
  },
  testRegex: "(<rootDir>/__tests__/.*|(\\.|/)(test|spec))\\.([tj]sx?)$",
  moduleNameMapper: {
    ...pathsToModuleNameMapper(compilerOptions.paths),
    "\\.(css|less)$": "identity-obj-proxy",
    "\\.(jpg|jpeg|png|gif|eot|otf|webp|svg|ttf|woff|woff2|mp4|webm|wav|mp3|m4a|aac|oga)$":
      "mocks/file-mock.js",
  },
  moduleFileExtensions: ["ts", "tsx", "js", "jsx", "json", "node"],
  testPathIgnorePatterns: ["node_modules", ".cache"],
  transformIgnorePatterns: ["node_modules/(?!(gatsby)/)"],
  testURL: "http://localhost",
  modulePaths: ["<rootDir>"],
  moduleDirectories: [".", "src", "src/util", "node_modules"],
  rootDir: "../",
}
