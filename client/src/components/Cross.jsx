/*import React from "react";
import { View, StyleSheet } from "react-native";

const Cross = () => {
  return (
    <View style={styles.cross}>
      <View style={styles.crossLine} />
      <View style={[styles.crossLine, styles.crossLineReversed]} />
    </View>
  );
};

const styles = StyleSheet.create({
  cross: {
    flex: 1,
  },
  crossLine: {
    position: "absolute",
    left: "48%",
    width: 10,
    height: "100%",
    backgroundColor: "white",
    borderRadius: 5,
    transform: [
      {
        rotate: "45deg",
      },
    ],
  },
  crossLineReversed: {
    transform: [
      {
        rotate: "-45deg",
      },
    ],
  },
});

export default Cross;*/

import React from "react";
import { View, StyleSheet } from "react-native";

const Cross = () => {
  return (
    <View style={styles.cross}>
      <View style={[styles.line, styles.line1]} />
      <View style={[styles.line, styles.line2]} />
    </View>
  );
};

const styles = StyleSheet.create({
  cross: {
    flex: 1,
    alignItems: "center",
    justifyContent: "center",
   // position: "relative", // ✅ Needed for absolute children
    margin: 10,
   // backgroundColor: "rgba(255,0,0,0.2)", // debug
  },
  line: {
    position: "absolute",
    width: 10,
    height: 100,
    backgroundColor: "white",
    borderRadius: 5,
  },
  line1: {
    transform: [{ rotate: "45deg" }],
  },
  line2: {
    transform: [{ rotate: "-45deg" }],
  },
});

export default Cross;

