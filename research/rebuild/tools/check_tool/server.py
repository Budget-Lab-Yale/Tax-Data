"""server.py: the check tool's web server (README.md). It reads the tables of records prepare.py wrote for the year in
settings.json, once at start, and answers each request from the page with a table: an item Y by a variable X, in
the item's source and in the posttrained file, side by side. It sends tables only, never records.

Every request must carry the token printed at start (a random string made anew each time the server starts): the
page sends it with each request, and a request without it is refused. Needs only Python 3.12 and the pandas, numpy
and pyarrow of the Python/3.12.3 and Arrow/17.0.0 modules.

    python server.py [--port N] [--cache DIR]      (start.sbatch runs it in a Slurm job)
"""
import argparse
import hmac
import http.server
import json
import os
import random
import re
import secrets
import socket
import sys
import time
import urllib.parse

from check_tables import HERE, SETTINGS, Data, QueryError, default_folder, jsonable, run_query


# ---- the web server ------------------------------------------------------------------------------------------------
PREFIX = re.compile(r'^/r?node/[^/]+/\d+')      # Open OnDemand's node proxy passes /node/<host>/<port>/ through


def make_handler(data, token):
    page = open(os.path.join(HERE, 'index.html'), 'rb').read()

    class Handler(http.server.BaseHTTPRequestHandler):
        server_version = 'check_tool'
        sys_version = ''

        def log_message(self, fmt, *args):    # never the token or the query: only the path and the status
            path = urllib.parse.urlsplit(self.path).path
            sys.stderr.write(f'{time.strftime("%H:%M:%S")} {self.command} {path} {args[1] if len(args) > 1 else ""}\n')

        def authorized(self):
            parts = urllib.parse.urlsplit(self.path)
            given = self.headers.get('X-Check-Token') or urllib.parse.parse_qs(parts.query).get('token', [''])[0]
            return hmac.compare_digest(given.encode(), token.encode())

        def send(self, code, body, ctype='application/json'):
            if isinstance(body, (dict, list)):
                body = json.dumps(jsonable(body)).encode()
            elif isinstance(body, str):
                body = body.encode()
            self.send_response(code)
            self.send_header('Content-Type', ctype + ('; charset=utf-8' if 'json' in ctype or 'text' in ctype else ''))
            self.send_header('Content-Length', str(len(body)))
            self.send_header('Cache-Control', 'no-store')
            self.send_header('X-Content-Type-Options', 'nosniff')
            self.send_header('Referrer-Policy', 'no-referrer')
            self.end_headers()
            self.wfile.write(body)

        def route(self):
            return PREFIX.sub('', urllib.parse.urlsplit(self.path).path) or '/'

        def do_GET(self):
            if not self.authorized():
                return self.send(403, 'Refused: this page needs the token printed when the server started.', 'text/plain')
            r = self.route()
            if r in ('/', '/index.html'):
                return self.send(200, page, 'text/html')
            if r.endswith('/api/catalog') or r == '/api/catalog':
                c = dict(data.catalog)
                c['load_seconds'] = data.load_seconds
                return self.send(200, c)
            return self.send(404, 'not found', 'text/plain')

        def do_POST(self):
            if not self.authorized():
                return self.send(403, 'Refused: this page needs the token printed when the server started.', 'text/plain')
            if not self.route().endswith('/api/table'):
                return self.send(404, 'not found', 'text/plain')
            try:
                n = int(self.headers.get('Content-Length', '0'))
                if n > 100_000:
                    raise QueryError('request too long')
                q = json.loads(self.rfile.read(n) or b'{}')
                return self.send(200, run_query(data, q))
            except QueryError as e:
                return self.send(400, {'error': str(e)})
            except Exception as e:          # a bug: say what, never a record
                return self.send(500, {'error': f'{type(e).__name__}: {e}'})

    return Handler


def free_port(lo, hi):
    for _ in range(200):
        p = random.randint(lo, hi)
        with socket.socket() as s:
            try:
                s.bind(('', p))
                return p
            except OSError:
                continue
    raise SystemExit('no free port found')


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--port', type=int, default=0)
    ap.add_argument('--cache', default=None, help='the folder prepare.py wrote (default: from settings.json)')
    a = ap.parse_args()
    folder = a.cache or default_folder()
    data = Data(folder)
    token = secrets.token_urlsafe(24)
    port = a.port or free_port(*SETTINGS['port_range'])
    srv = http.server.ThreadingHTTPServer(('', port), make_handler(data, token))
    node = socket.gethostname().split('.')[0]
    tok_file = os.environ.get('CHECK_TOOL_TOKEN_FILE')
    if tok_file:                                          # for the test job: readable by the owner only
        fd = os.open(tok_file, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o600)
        os.write(fd, json.dumps({'node': node, 'port': port, 'token': token}).encode())
        os.close(fd)
    print(f"""
The check tool is running: {data.catalog['year']} file, source definition {data.catalog['puf_source']}.
Tables loaded from {folder} in {data.load_seconds:.0f} seconds.

  node   {node}
  port   {port}
  token  {token}

Open it in one of these ways (README.md, "Opening the page"):
  1. Open OnDemand Remote Desktop: in the desktop's web browser, open
       http://{node}:{port}/?token={token}
  2. Open OnDemand's node proxy, if YCRC has it switched on for this node:
       https://ood-bouchet.ycrc.yale.edu/rnode/{node}/{port}/?token={token}
  3. An SSH tunnel from your own computer: run
       ssh -N -L {port}:{node}:{port} <your NetID>@bouchet.ycrc.yale.edu
     and then open http://localhost:{port}/?token={token}

Stop the server with: scancel {os.environ.get('SLURM_JOB_ID', '<job id>')}
""", flush=True)
    srv.serve_forever()


if __name__ == '__main__':
    main()
