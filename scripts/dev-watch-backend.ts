import fs from 'fs/promises';
import { constants } from 'fs';
import path from 'path';
import child_process from 'child_process';
import chalk from 'chalk';

function copy(e: Array<Executable>): Array<Executable> {
  const clone = Array<Executable>();

  for (let i = 0; i < e.length; i++) {
    const element = e[i];
    clone.push({
      fullPath: element.fullPath,
      mtime: new Date(element.mtime)
    });
  }

  return clone;
}

type Executable = {
  fullPath: string;
  mtime: Date;
};

async function findAllExecutables(dir: string): Promise<Array<Executable>> {
  let results = new Array<Executable>();
  const contents = await fs.readdir(dir);

  for (let i = 0; i < contents.length; i++) {
    const element = contents[i];
    const fullPath = path.join(dir, element);

    const stat = await fs.stat(fullPath);

    if (stat.isDirectory()) {
      results.push(...await findAllExecutables(fullPath));
    } else {
      if (stat.isFile()) {
        try {
          await fs.access(fullPath, constants.X_OK);
          results.push({
            fullPath,
            mtime: new Date(stat.mtime)
          });
        } catch (e) {
          // ignore
        }
      }
    }
  }

  return results;
}

function getLastModified(executables: Array<Executable>): Executable | undefined {
  const lastModified = copy(executables).sort((a, b) => new Date(b.mtime).getTime() - new Date(a.mtime).getTime());

  if (lastModified.length > 0) {
    return lastModified[0];
  }

  return undefined;
}

function didExecutableChange(current: Executable | undefined, next: Executable): boolean {
  if (current === undefined) {
    return true;
  }

  return current.fullPath !== next.fullPath || current.mtime.getTime() < next.mtime.getTime();
}

class ExecutableWatcher {
  private currentExecutable: Executable | undefined;
  private existingProcess: child_process.ChildProcess | undefined;

  constructor() {
    this.currentExecutable = undefined;
  }

  public start() {
    setInterval(async () => {
      const latestExecutable = getLastModified(await findAllExecutables('.stack-work'));

      if (latestExecutable !== undefined && didExecutableChange(this.currentExecutable, latestExecutable)) {
        this.currentExecutable = latestExecutable;
        this.onChange();
      }
    }, 5000);
  }

  private onChange() {
    if (this.currentExecutable !== undefined) {
      console.log(chalk.gray(new Date().toISOString()));
      if (this.existingProcess !== undefined) {
        this.existingProcess.kill();
        console.log(`${chalk.red('Restarting executable')}: ${this.currentExecutable.fullPath}`);
      } else {
        console.log(`${chalk.red('Starting executable')}: ${this.currentExecutable.fullPath}`);
      }
      console.log();

      const port = process.env.PORT || '8080';

      const newProc = child_process.spawn(this.currentExecutable.fullPath, ['--port', port, "--access-log", "-", "--error-log", 'stderr'], {
        env: {
          CONNECT_BASE_URL: process.env.CONNECT_BASE_URL
        },
      });

      newProc.stdout.on('data', data => {
        process.stdout.write(`${data}`);
      });

      newProc.stderr.on('data', data => {
        process.stderr.write(`${data}`);
      });

      this.existingProcess = newProc;
    }
  }
}

async function main() {
  new ExecutableWatcher().start();
}

main().catch(e => console.error(e));