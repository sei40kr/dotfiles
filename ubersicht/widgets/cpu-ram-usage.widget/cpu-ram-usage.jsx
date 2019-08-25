export const command = `{ sysctl hw.ncpu; ps aux; } | awk 'NR == 1 { ncpu = $2 } NR != 1 { cpu += $3; ram += $4 } END { print cpu / ncpu, ram }'`;

export const refreshFrequency = 1000;

const usageFormatter = new Intl.NumberFormat('en-US', {
  minimumFractionDigits: 1,
  maximumFractionDigits: 1,
});

const renderCpuUsage = (cpuUsage) => `CPU: ${usageFormatter.format(cpuUsage)}`;
const renderRamUsage = (ramUsage) => `RAM: ${usageFormatter.format(ramUsage)}`;

export const render = ({ output }) => {
  if (!output) {
    return null;
  }

  const [cpuUsage, ramUsage] = output.split(/\s+/);

  return [renderCpuUsage(cpuUsage), <span class="divider">/</span>, renderRamUsage(ramUsage)];
};

export const className = `
bottom: 0;
color: #bfbfbf;
font-family: Menlo, monospace;
font-size: 16px;
height: 32px;
line-height: 32px;
position: absolute;
right: 1vw;
text-align: right;
z-index: 100;

.divider {
  &::before {
    content: '';
    margin-left: 0.5em;
  }

  &::after {
    content: '';
    margin-right: 0.5em;
  }
}
`;
