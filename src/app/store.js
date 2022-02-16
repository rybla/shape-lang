import { configureStore } from "@reduxjs/toolkit";
import mainSlice from "../features/main/mainSlice"
import programSlice from "../features/program/programSlice";

export default configureStore({
  reducer: {
    app: mainSlice.reducer,
    program: programSlice.reducer
  }
})
