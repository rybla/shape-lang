const controls: JSX.Element = (
  <div id="controls" style={{ display: "none" }}>
    <h2>Controls</h2>
    <h3>Term mode</h3>
    <h4>Placement</h4>
    <table>
      <thead>
        <tr>
          <th>key</th>
          <th>behavior</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <td>
            <code>u</code>
          </td>
          <td>place U (universe levels not implemented yet)</td>
        </tr>
        <tr>
          <td>
            <code>p</code>
          </td>
          <td>place Π (auto-focus on binding)</td>
        </tr>
        <tr>
          <td>
            <code>l</code>
          </td>
          <td>place λ (auto-focus on binding)</td>
        </tr>
        <tr>
          <td>
            <code>=</code>
          </td>
          <td>place let (auto-focus on binding)</td>
        </tr>
        <tr>
          <td>
            <code>Backspace</code>
          </td>
          <td>place hole (i.e. dig i.e. delete)</td>
        </tr>
      </tbody>
    </table>
    <h4>Navigation</h4>
    <table>
      <thead>
        <tr>
          <th>key</th>
          <th>behavior</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <td>
            <code>LeftArrow</code>, <code>Shift+Tab</code>
          </td>
          <td>go left (or, go up, if focus is locally left-most)</td>
        </tr>
        <tr>
          <td>
            <code>RightArrow</code>, <code>Tab</code>
          </td>
          <td>
            go right (or, go up then right, if focus is locally right-most)
          </td>
        </tr>
        <tr>
          <td>
            <code>UpArrow</code>
          </td>
          <td>go up</td>
        </tr>
        <tr>
          <td>
            <code>DownArrow</code>
          </td>
          <td>go down (to left-most subnode)</td>
        </tr>
        <tr>
          <td>
            <code>x</code>
          </td>
          <td>enter binding mode for current focus (only for Π, λ, let)</td>
        </tr>
      </tbody>
    </table>
    <h3>Binding mode</h3>
    <table>
      <thead>
        <tr>
          <th>key</th>
          <th>behavior</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <td>char</td>
          <td>append char to binding label</td>
        </tr>
        <tr>
          <td>
            <code>Enter</code>
          </td>
          <td>return to term mode</td>
        </tr>
        <tr>
          <td>navigation</td>
          <td>return to term mode, then perform navigation</td>
        </tr>
      </tbody>
    </table>
  </div>
);

export const help: JSX.Element = (
  <div id="help">
    <h1
      id="help-title"
      onClick={(event) => {
        let e = document.getElementById("controls");
        if (e) {
          if (e.style.display === "block") e.style.display = "none";
          else e.style.display = "block";
        }
      }}
    >
      Help (click: toggle display)
    </h1>
    {controls}
  </div>
);
