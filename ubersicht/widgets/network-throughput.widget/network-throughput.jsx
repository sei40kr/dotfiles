export const command = './network-throughput.widget/libexec/network-throughput';

export const refreshFrequency = 1000;

const valueFormatter = new Intl.NumberFormat('en-US', {
  useGrouping: false,
  minimumFractionDigits: 1,
  maximumFractionDigits: 1,
});

const humanize = (bytesPerSecond) => {
  let value = bytesPerSecond / 1024;
  let unit = 'kB/s';

  if (1024 <= value) {
    value /= 1024;
    unit = 'MB/s';
  }
  if (1024 <= value) {
    value /= 1024;
    unit = 'GB/s';
  }

  return valueFormatter.format(value) + unit;
};

const renderDownloadThroughput = (downloadThroughput) => (
  <div key="download" className="indicator indicator--download">
    <span className="indicator__icon"></span>
    <span className="indicator__value">{humanize(downloadThroughput)}</span>
  </div>
);

const renderUploadThroughput = (uploadThroughput) => (
  <div key="upload" className="indicator indicator--upload">
    <span className="indicator__icon"></span>
    <span className="indicator__value">{humanize(uploadThroughput)}</span>
  </div>
);

export const render = ({ output }) => {
  if (!output) {
    return null;
  }

  const lines = output.split(/\r\n|\r|\n/);
  const [downloadThroughput0, uploadThroughput0] = lines[0].split(/\s+/);
  const [downloadThroughput1, uploadThroughput1] = lines[1].split(/\s+/);

  return [
    renderDownloadThroughput(downloadThroughput1 - downloadThroughput0),
    renderUploadThroughput(uploadThroughput1 - uploadThroughput0),
  ];
};

export const className = `
-webkit-font-smoothing: subpixel-antialiased;
display: flex;
font-family: 'Input Mono', monospace;
font-size: 16px;
position: absolute;
right: calc(1vw + 351px + 1em);
text-align: right;
top: 0;
z-index: 100;

.indicator {
  border-bottom: 3px solid transparent;
  height: 29px;
  line-height: 29px;
  padding: 0 0.5em;

  &:not(:last-child) {
    margin-right: 0.5em;
  }

  &--download {
    border-color: #98be65;
    color: #98be65;
  }

  &--upload {
    border-color: #ff6c6b;
    color: #ff6c6b;
  }

  &__icon {
    -webkit-font-smoothing: antialiased;
    font-family: 'Font Awesome 5 Free Solid';
    font-size: 1.1em;
  }

  &__value {
    margin-left: 0.5em;
  }
}
`;
