const http = require('http')
const elmReload = require('./elm-reload')
const fs = require('fs')
const opn = require('opn')
const clc = require('cli-color')
const argv = require('minimist')(process.argv.slice(2))

const serveStatic = require('serve-static')
const finalhandler = require('finalhandler')
const URL = require('url-parse')

const port = argv._[0]
const dir = argv._[1]
const openBrowser = argv._[2] === 'true'
const hostname = argv._[3]
const runFile = argv._[4]
const startPage = argv._[5]
const pushstate = argv._[6] === 'true'
const verbose = argv._[7] === 'true'
const proxyPrefix = argv._[8]
const proxyHost = argv._[9]

const reloadOpts = {
  port: port,
  verbose: verbose
}

let reloadReturned

const serve = serveStatic(dir, { index: ['index.html', 'index.htm'] })

let proxy
if (typeof proxyPrefix === 'string' && typeof proxyHost === 'string') {
  proxy = require('http-proxy').createProxyServer()
}

const server = http.createServer(function (req, res) {
  const url = new URL(req.url)

  if (proxy && url.pathname.startsWith(proxyPrefix)) {
    proxy.web(req, res, { target: proxyHost })
    return
  }

  const pathname = url.pathname.replace(/(\/)(.*)/, '$2') // Strip leading `/` so we can find files on file system
  const fileEnding = pathname.split('.')[1]

  if (
    (pushstate && fileEnding === undefined) ||
    fileEnding === 'html' ||
    pathname === '/' ||
    pathname === ''
  ) {
    const finalpath =
      pathname === '/' || pathname === ''
        ? dir + '/' + startPage
        : dir + '/' + pathname

    fs.readFile(finalpath, 'utf8', function (err, contents) {
      if (err) {
        const rootpath = dir + '/' + startPage
        fs.readFile(rootpath, 'utf8', function (err, contents) {
          if (err) {
            res.writeHead(404, { 'Content-Type': 'text/plain' })
            res.end('File Not Found')
          } else {
            res.setHeader('Content-Type', 'text/html')
            res.end(
              `${contents} \n\n<!-- Inserted by Reload -->\n<script src="/reload/reload.js"></script>\n<!-- End Reload -->\n`
            )
          }
        })
      } else {
        res.setHeader('Content-Type', 'text/html')
        res.end(
          `${contents} \n\n<!-- Inserted by Reload -->\n<script src="/reload/reload.js"></script>\n<!-- End Reload -->\n`
        )
      }
    })
  } else if (pathname === 'reload/reload.js') {
    // Server reload-client.js file from injected script tag
    res.setHeader('Content-Type', 'text/javascript')
    res.end(reloadReturned.reloadClientCode())
  } else {
    // Serve any other file using serve-static
    serve(req, res, finalhandler(req, res))
  }
})

// Reload call and configurations. Stub app as it isn't used here
reloadReturned = elmReload(reloadOpts, server)

server.listen(port, function () {
  if (!fs.existsSync(runFile)) {
    fs.writeFileSync(runFile)

    // If openBrowser, open the browser with the given start page above, at a hostname (localhost default or specified).
    if (openBrowser) {
      opn('http://' + hostname + ':' + port)
    }
  } else {
    const time = new Date()
    console.log(
      clc.green('Server restarted at ' + time.toTimeString().slice(0, 8))
    )
  }
})
