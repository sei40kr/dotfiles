module.exports = {
  // Define a list of glob patterns that match files and directories that you don't want to include for importing.
  excludes: ['**/build/**'],

  // This list of environments controls what core modules are available when importing, and what variables are considered global by default. The supported values right now are
  enviroments: ['browser', 'jest'],

  // By default, ImportJS will put imports into groups:
  //   1. Core modules
  //   2. Package dependencies
  //   3. One or more groups with internal imports
  // You can turn off this behavior by setting groupImports to false. When disabled, imports are listed alphabetically in one list.
  groupImports: false,

  // By default, ImportJS will sort imports by the name or path of the imported module.
  // You can turn off this behavior by setting sortImports to false. When disabled, existing imports are not rearranged, and new imports are always added above existing imports.
  sortImports: false,

  // An array that controls what file extensions are stripped out from the resulting import statement. The default configuration strips out [".js", ".jsx"]. Set to an empty array [] to avoid stripping out extensions.
  stripFileExtensions: ['.js', '.jsx', '.ts', '.tsx']
};
