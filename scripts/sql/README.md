# Migrations and things.

So apparently, like, you can't just set your data once and have it work out
beautifully. You have to, like, make changes to it and stuff.

So here be the Migrations instructions:

* Ensure you have your Postgres variables set (i.e. `PGHOST`, `PGUSER`,
  `PGPASSWORD`, `PGDATABASE`).
* Run `node .` or `node index.js` to "run" them.

You can, like the folks I work for, use `node -c "name"` to create a new one.

### What it does.

Checks what version you are on. Runs through the filesystem, finds all more
recent versions, and executes them, then updates the DB with the changes, then
increments the version.

### Bugs?

Oh hell yeah baby. It doesn't fail gracefully and I'm pretty sure it does other
things incorrectly. Grow this shit.
