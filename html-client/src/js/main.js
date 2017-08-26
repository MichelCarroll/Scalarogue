import React, { PropTypes, Component} from 'react'

module.exports = class App extends Component {
  
  constructor() {
    super();
    this.state = {};
  }
  
  componentDidMount() {
    window.Main().main({
      seed: +(new Date()),
      viewport: document.getElementById('viewport'),
      minimap: document.getElementById('minimap'),
      onUpdateState: (state) => {
        console.log(state);
        this.setState(state)
      }
    });
  }
  
  renderStats() {
    if(!this.state.stats) {
      return null;
    }
    
    const healthCurrent = this.state.stats.health.current;
    const healthMax = this.state.stats.health.max;
    const healthPerc = healthCurrent / healthMax * 100;
    
    return <div className="section stats-section">
        <ul id="stats-list">
            <li>
                <div className="stat-name">Health</div>
                <div className="stat-bar-outer">
                    <div className="stat-bar-inner" style={{background: 'red', width: healthPerc + "%"}}></div>
                    <div className="stat-bar-annotation">
                      {healthCurrent+"/"+healthMax}
                    </div>
                </div>
            </li>
        </ul>
    </div>
  }
  
  renderNotifications() {
    if(!this.state.notifications) {
      return null;
    }
    
    return <div id="message-container" className="message-container" width="600" height="200">
        <ul id="messages" className="messages">{
          this.state.notifications.map((notification, i) => <li key={i}>{notification}</li>)
        }</ul>
    </div>
  }
  
  render() {
    return <div>
      <canvas className="section" width="600" height="600" id="viewport"></canvas>
      <canvas className="section" width="200" height="200" id="minimap"></canvas>
      {this.renderStats()}
      {this.renderNotifications()}
    </div>
  }
}
