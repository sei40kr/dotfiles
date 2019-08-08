export const command = '/usr/local/bin/yabai -m query --windows --window';

export const refreshFrequency = false;

const renderActiveWindow = (activeWindow) => activeWindow.title;

export const render = ({ output }) => {
  if (!output) {
    return null;
  }

  const activeWindow = JSON.parse(output);

  return renderActiveWindow(activeWindow);
};

export const className = `
color: #bfbfbf;
font-family: Menlo, monospace;
font-size: 16px;
height: 32px;
left: 34.5vw;
line-height: 32px;
overflow: hidden;
position: absolute;
right: 34.5vw;
text-align: center;
text-overflow: ellipsis;
top: 0;
white-space: nowrap;
z-index: 100;
`;
