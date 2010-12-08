#!/usr/bin/env node

// Force node to run code from standard input.
var stdin = process.openStdin()
  , buf = '';
stdin.setEncoding('utf8');
stdin.on('data', function(chunk) { buf += chunk });
stdin.on('end', function() { eval(buf) });
