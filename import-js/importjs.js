module.exports = {
  excludes: ['**/build/**'],
  environments: ['node', 'browser', 'jest'],
  groupImports: false,
  sortImports: false,
  emptyLineBetweenGroups: true,
  importDevDependencies: false,
  danglingCommas: false,
  stripFileExtensions: ['.js', '.jsx', '.ts', '.tsx'],
  useRelativePaths: true,
  maxLineLength: 80,
  tab: '  ',
  logLevel: 'info',
};
