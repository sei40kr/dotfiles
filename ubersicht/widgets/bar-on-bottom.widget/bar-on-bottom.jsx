export const command = './bar-on-bottom.widget/libexec/chunkwm-status';

export const refreshFrequency = 500; // ms

const renderDesktop = ({ desktopId, isActive }) => {
  let desktopName;

  switch (desktopId) {
    case '1':
      desktopName = 'Web';
      break;
    case '2':
      desktopName = 'File';
      break;
    case '3':
      desktopName = 'Coding';
      break;
    case '4':
      desktopName = 'Design';
      break;
    case '5':
      desktopName = 'Messenger';
      break;

    default:
      desktopName = `Desktop ${desktopId}`;
  }

  return (
    <span
      key={desktopId}
      className={`desktop ${isActive ? 'desktop--active' : ''}`}
    >
      {desktopName}
    </span>
  );
};

const renderDesktops = (activeDesktopId, desktopIds) => (
  <div className="desktops">
    {desktopIds.map((desktopId) =>
      renderDesktop({ desktopId, isActive: desktopId === activeDesktopId })
    )}
  </div>
);

export const render = ({ output }) => {
  if (!output) {
    return null;
  }

  const lines = output.split(/\r\n|\r|\n/);

  const desktopIds = lines[0].split(/\s+/);
  const activeDesktopId = lines[1];

  return renderDesktops(activeDesktopId, desktopIds);
};

export const className = `
align-items: center;
backdrop-filter: blur(20px);
background: rgba(0, 0, 0, .1);
bottom: 0;
box-sizing: border-box;
color: #fff;
display: flex;
font-family: 'SF Display', sans-serif;
font-size: 15px;
-webkit-font-smoothing: antialiased;
height: calc(56px + 60px);
justify-content: center;
left: 0;
padding: 0 0 56px;
position: absolute;
right: 0;
width: 100%;

.desktops {
  display: flex;
  margin: 3px 0 0;
}

.desktop {
  border-radius: 4px;
  display: block;
  height: 1.5em;
  line-height: 1.5em;
  padding: 0 .8em;

  &:not(:last-child) {
    margin-right: .8em;
  }

  &--active {
    background: rgba(255, 255, 255, .1);
  }
}
`;
