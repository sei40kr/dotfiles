export const command = `
./bar-on-bottom.widget/libexec/cpu-and-disk-usage
./bar-on-bottom.widget/libexec/ram-usage
`;

export const refreshFrequency = 1500; // ms

const Stat = ({ label, children }) => (
  <div className="stat">
    <div className="stat-label">{label}</div>
    <div className="stat-content">{children}</div>
  </div>
);

export const render = ({ output }) => {
  if (!output) {
    return null;
  }

  const lines = output.split(/\r\n|\r|\n/);
  const [cpuUsagePercent, diskUsage] = lines[0].split(/\s+/);
  const [ramUsage, ramTotal] = lines[1].split(/\s+/);

  return (
    <div className="bar-on-bottom">
      <Stat label="CPU">
        <div className="usage">
          <span className="value">{cpuUsagePercent}</span>
          <span className="unit">%</span>
        </div>
      </Stat>
      <Stat label="RAM">
        <div className="usage">
          <span className="numerator">{ramUsage}</span>
          <span className="denominator">{ramTotal}</span>
          <span className="unit">MB</span>
        </div>
      </Stat>
      <Stat label="Disk">
        <div className="usage">
          <span className="value">{diskUsage}</span>
          <span className="unit">MB/s</span>
        </div>
      </Stat>
    </div>
  );
};

export const className = `
bottom: 64px;
left: 0;
position: absolute;
right: 0;
width: 100%;

.bar-on-bottom {
  align-items: center;
  color: #fff;
  display: flex;
  font-family: 'Helvetica Neue', sans-serif;
  font-size: 15px;
  height: 48px;
  justify-content: flex-end;
  padding: 0 42px;
}

.stat {
  align-items: center;
  display: flex;

  &:not(:last-child) {
    margin-right: 2em;
  }

  > .stat-label {
    margin-right: 2em;
    opacity: 0.4;
    text-align: center;
    text-transform: uppercase;
  }
}

.usage {
  .value {
    font-size: 1.75em;
  }
  .numerator {
    font-size: 1.75em;
  }
  .denominator {
    &::before {
      content: '/';
      margin-left: 0.5em;
      margin-right: 0.5em;
    }
  }
  .unit {
    margin-left: 0.25em;
  }
}
`;
