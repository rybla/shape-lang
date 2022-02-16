import "./Main.module.css"
import { useDispatch, useSelector } from "react-redux";
import mainSlice from "./mainSlice";

export default function Main() {
  const testValue = useSelector(state => state.app.testValue)
  const dispatch = useDispatch()

  return (
    <div>
      <div>
        {testValue}
      </div>
      <div>
        <button onClick={() => dispatch(mainSlice.actions.testAction())}>Test</button>
      </div>
    </div>
  )
}