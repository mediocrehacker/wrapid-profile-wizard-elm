const webpack = require('webpack');
const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');

module.exports = function (config) {
  return {
    entry: {
      index: path.join(config.src, 'index.js')
    },

    plugins: [
      new HtmlWebpackPlugin({
        title: 'Webpack demo'
      }),
      new webpack.IgnorePlugin(/\/iconv-loader$/)
    ],

    module: {
      rules: [
        {
          test: /\.json($|\?)/,
          use: 'json-loader'
        },
        {
          test: /\.(jpg|png|svg|gif)$/,
          include: config.src,
          use: [
            { loader: 'file-loader',
              options: { name: './images/[name].[hash].[ext]' }
            }
          ]
        }
      ]
    }
  };
};
