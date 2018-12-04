#!/usr/bin/env node

var program = require('commander')
var path = require('path')

program
  .version(require('../package.json').version)
  .option('-o, --open', 'Open in the browser automatically.')
  .option(
    '-n, --host [host]',
    'If -b flag is being used, this allows for custom hostnames. Defaults to localhost.',
    'localhost'
  )
  .option(
    '-d, --dir [dir]',
    'The directory to serve up. Defaults to current dir.',
    process.cwd()
  )
  .option(
    '-w, --watch-dir [watch-dir]',
    'The directory to watch. Defaults the serving directory.'
  )
  .option(
    '-e, --exts [extensions]',
    'Extensions separated by commas or pipes. Defaults to html,js,css.',
    'html|js|css'
  )
  .option(
    '-p, --port [port]',
    'The port to bind to. Can be set with PORT env variable as well. Defaults to 1234',
    '1234'
  )
  .option(
    '-x, --proxyPrefix [prefix]',
    'Proxy requests to paths starting with `prefix` to another server. Requires `--proxyHost` and should be a string like `/api`. Defaults to not proxying'
  )
  .option(
    '-y, --proxyHost [proxyhost]',
    'Proxy requests to another server running at `host`. Requires `--proxyHost` and should be a full URL, eg. `http://localhost:9000`. Defaults to not proxying'
  )
  .option(
    '-s, --start-page [start-page]',
    'Specify a start page. Defaults to index.html',
    'index.html'
  )
  .option(
    '-u, --pushstate [pushstate]',
    'Automatically serve the root or `index.html` for SPAs. Defaults to false.',
    false
  )
  .option(
    '-v, --verbose [verbose]',
    'Turning on logging on the server and client side. Defaults to false',
    false
  )
  .parse(process.argv)

var elmServe = require(path.join(__dirname, '../lib/elm-serve.js'))
elmServe(program)
