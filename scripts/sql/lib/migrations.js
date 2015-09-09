/*
 * Naming conventions is
 */
const _ = require('lodash');
const async = require('async');
const fs = require('fs');
const moment = require('moment');
const nconf = require('nconf');
const path = require('path');
const pg = require('pg');
const util = require('util');

nconf.argv().env();
const MAX_TITLE_LENGTH = 30;
const MOMENT_DATE_FORMAT = 'YYYYMMDD-HHmmss';

let client = 'UNINITIALIZED';
let lastVersion = 'UNINITIALIZED';


/*
 * Reads the version from 'migrations' table.
 * Scans the list of files that were created after the version of the DB.
 * Executes them in order.
 * Sets a new version.
 */
function runMigrations(cb) {
  async.waterfall([
      getCutoff,
      getMigrationsAfter,
      function(migrationsAfter, callback) {
        const asyncExecution = _.map(migrationsAfter, execFileMapper);
        async.series(asyncExecution, callback);
      },
      (data, callback) => { 
        if (data === []) {
          callback(null, null);
        } else {
          incrementVersion(callback);
        }
      },
      (_data, callback) => {
        client.end();
        console.log('FINISHED!');
        callback(null); 
      }
  ], cb);
}

function getCutoff(cb) {
  setPostgresClient();

  client.connect( (err) => {
    if (err) return die('Failure to connect', err);
    client.query('SELECT version, run_on FROM migrations ORDER BY run_on DESC LIMIT 1', (err, result) => {
      lastVersion = result.rows[0].version;
      const cutoff = moment(result.rows[0].run_on);
      console.log('Found last migration run at ', cutoff.format(MOMENT_DATE_FORMAT), ' version ', lastVersion);
      cb(null, cutoff);
    });
  });
}


function getMigrationsAfter(cutoff, cb) {
  // get the files in migrations, sort by date.
  const contents = fs.readdirSync('migrations');
  const after = _.chain(contents)
    .filter((filename) => {
      const dateSubstr = filename.substr(0, MOMENT_DATE_FORMAT.length);
      const dateFromFile = moment(dateSubstr, MOMENT_DATE_FORMAT);
      return dateFromFile.isAfter(cutoff);
    })
    .sortBy()
    .map( filename => path.join('migrations', filename) )
    .value();
  console.log('Will run the following migrations:', after);
  cb(null, after);
}


function execFileMapper(filename) {
  return (cb) => {
    console.log('Executing', filename, '…');
    fs.readFile(filename, 'utf-8', (err, data) => {
      if (err) {
        console.error('ERROR READING MIGRATION FILE:', err);
        return cb(err);
      }
      client.query(data, cb);
    });
  };
}

function incrementVersion(cb) {
  client.query('INSERT INTO migrations(version) VALUES($1)', [lastVersion + 1], cb);
}


function setPostgresClient() {
  const user = nconf.get('PGUSER'); 
  const password = nconf.get('PGPASSWORD'); 
  const host = nconf.get('PGHOST'); 
  const db = nconf.get('PGDATABASE'); 
  const conString = util.format('postgres://%s:%s@%s/%s', user, password, host, db);
  client = new pg.Client(conString);
}


function die(msg) {
  console.error(msg);
  process.exit(1);
}


/*
 * Adds a new file with the specified name and the time it was created.
 */
function createMigration(migrationName) {
  const migrationWithDashes = migrationName.substr(0, MAX_TITLE_LENGTH).replace(/ /g, '-').toLowerCase();
  const formatString = moment().format(MOMENT_DATE_FORMAT);
  const filename = formatString + '-' + migrationWithDashes + '.sql';
  const fullFilename = path.join('migrations', filename);

  fs.writeFileSync(fullFilename, '-- Migration: ' + migrationName);
  console.log('Wrote out new file:', fullFilename);
}



///////////////////////////////////////////////////////////////////////////////
//    OH HELL YEAH MAINLINE!!!
///////////////////////////////////////////////////////////////////////////////

const migrationName = nconf.get('c');
if (migrationName) {
  createMigration(migrationName);
  process.exit(0);
} else {
  runMigrations(function(err) {
    if (err) console.error(err);  
    process.exit(0);
  });
}


