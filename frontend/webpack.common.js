const path = require('path');
const webpack = require('webpack');
const HtmlWebpackPlugin = require('html-webpack-plugin');

module.exports = {
  entry: './src/index.tsx',
  mode: 'development',
  target: 'web',
  module: {
    rules: [
      {
        test: /\.m?js/,
        resolve: {
          fullySpecified: false
        }
      },
      {
        test: /\.tsx?$/,
        use: 'babel-loader',
        exclude: /node_modules/,
      },
      {
        test: /\.css$/i,
        use: ["style-loader", "css-loader"],
      },
      {
        test: /\.js$/,
        use: ["source-map-loader"],
        enforce: "pre"
      },
      {
        test: /\.(png|svg|jpg|jpeg|gif|md)$/i,
        type: 'asset/resource',
      },
    ],
  },
  resolve: {
    extensions: ['.tsx', '.ts', '.js'],
  },
  output: {
    publicPath: '/static/frontend/',
    filename: 'bundle.[chunkhash].js',
    path: path.resolve(__dirname, 'dist'),
  },
  plugins: [
    new HtmlWebpackPlugin({
      title: 'Hackathon for Cloud',
      template: 'public/index.html',
      publicPath: '/static/frontend/',
      filename: 'index.html'
    }),
    new webpack.DefinePlugin({
      'process': undefined,
      'process.release': null,
      'setImmediate': undefined
    }),
    new webpack.EnvironmentPlugin({
      ANALYTICS_NEXT_MODERN_CONTEXT: true,
      NODE_ENV: 'development',
      CI: false
    }),
  ],
};