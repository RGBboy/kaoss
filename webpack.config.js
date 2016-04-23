module.exports = {
  entry: './src/client.js',

  output: {
    path: './build',
    filename: 'client.js'
  },

  resolve: {
    modulesDirectories: ['node_modules'],
    extensions: ['', '.js', '.elm']
  },

  module: {
    loaders: [
      {
        test: /\.html$/,
        exclude: /node_modules/,
        loader: 'file?name=[name].[ext]'
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader: 'elm-hot!elm-webpack'
      }
    ],

    noParse: /\.elm$/
  },

  devServer: {
    stats: 'errors-only'
  }
};
