import { createSlice } from "@reduxjs/toolkit";

export const mainSlice = createSlice({
  name: "main",
  initialState: {
    testValue: 0
  },
  reducers: {
    testAction: state => {
      console.log("testAction")
      state.testValue = state.testValue + 1
    }
  }
})

export const { testAction } = mainSlice.actions

export default mainSlice.reducer