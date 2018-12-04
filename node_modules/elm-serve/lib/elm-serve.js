module.exports = function elmServe (opts) {
  const supervisor = require('supervisor')
  const path = require('path')
  const os = require('os')
  const clc = require('cli-color')

  const runFile = path.join(
    os.tmpdir(),
    'reload-' +
      Math.random()
        .toString()
        .slice(2)
  )

  const serverFile = path.join(__dirname, './elm-reload-server.js')

  if (opts.proxyPrefix && !opts.proxyPrefix.startsWith('/')) {
    opts.proxyPrefix = opts.proxyPrefix + '/'
  }

  var args = [
    '-e',
    opts.exts || 'html|js|css',
    '-w',
    opts.watchDir || process.cwd(),
    '-q',
    '--',
    serverFile,
    opts.port || 1234,
    opts.dir || process.cwd(),
    opts.open || false,
    opts.host || 'localhost',
    runFile,
    opts.startPage || 'index.html',
    opts.pushstate || false,
    opts.verbose || false,
    opts.proxyPrefix || false,
    opts.proxyHost || false
  ]

  if ((opts.proxyPrefix && !opts.proxyHost) || (opts.proxyHost && !opts.proxyPrefix)) {
    throw new Error('If either `--proxyPrefix` and `--proxyHost` is given, the other must be as well')
  }

  supervisor.run(args)

  console.log('\nReload web server:')
  console.log('  - Website URL: ' + clc.blue.bold('http://' + (opts.host || 'localhost') + ':' + (opts.port || 1234)))
  console.log('  - Listening on port: ' + clc.cyan.bold(opts.port || 1234))
  console.log('  - Monitoring dir: ' + clc.green.bold(opts.dir || process.cwd()))
  if (opts.proxyPrefix && opts.proxyHost) {
    console.log('proxying requests starting with ' + clc.green(opts.proxyPrefix) + ' to ' + clc.green(opts.proxyHost))
  }
}
